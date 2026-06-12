# Java Data Service Provider Architecture

Date: 2026-06-12

## Conclusion

TeaQL Java should align conceptually with the Rust `teaql-data-service` model.

The Java runtime should not be centered on Spring beans, Spring annotations, JDBC, or SQL repositories. It should route query, aggregation, mutation, schema, and transaction operations through a Data Service provider architecture.

Spring, JDBC, Android SQLite, memory, Meilisearch, and future remote/search/vector backends are provider implementations or provider adapters. They are not the runtime center.

## Problem

The current Java runtime still reflects its Spring Boot origin:

- `TQLResolver` uses a bean-container shape: `getBean`, `getBeans`, `getBean(String)`.
- `GLobalResolver` provides a static runtime fallback.
- `teaql-sql` combines SQL compilation, repository behavior, Spring JDBC execution, and Spring transaction support.
- `teaql-sql-portable` is intended to support Android-style runtimes, but it still depends on `teaql-sql`, which depends on Spring JDBC.
- Non-Spring environments are currently framed as "use JDBC directly", which is too narrow.

Rust already has a broader abstraction: `teaql-data-service`. It can support SQL providers, memory providers, and providers like Meilisearch. Java should follow that direction.

## Design Principle

The Java runtime should be provider-oriented:

```text
Generated Q / Entity / Request API
        |
        v
UserContext
        |
        v
DataServiceRegistry
        |
        v
DataServiceExecutor
        |
        +-- SQL provider
        +-- Memory provider
        +-- Meilisearch provider
        +-- Remote provider
        +-- Future providers
```

SQL is one provider, not the runtime model.

JDBC is one SQL execution adapter, not the portable runtime model.

Spring is one framework adapter, not the runtime model.

## Rust Alignment

Java should mirror the core Rust concepts:

| Rust concept | Java target concept | Purpose |
| --- | --- | --- |
| `DataServiceExecutor` | `DataServiceExecutor` | Base capability-bearing executor |
| `QueryExecutor` | `QueryExecutor` | Execute generated query requests |
| `MutationExecutor` | `MutationExecutor` | Execute insert, update, delete, recover, batch |
| `TransactionExecutor` | `TransactionExecutor` | Begin provider-owned transaction |
| `SchemaExecutor` | `SchemaExecutor` | Ensure or inspect provider schema |
| `IdGeneratorExecutor` | `IdGeneratorExecutor` | Generate provider-backed IDs |
| `DataServiceCapabilities` | `DataServiceCapabilities` | Declare supported backend behavior |
| `ExecutionMetadata` | `ExecutionMetadata` | Record backend, operation, timing, trace, debug query |
| `EntityDescriptor.data_service` | `EntityDescriptor.dataService` | Route entity operations to a named provider |

Java does not need to copy Rust syntax, but it should preserve the same architectural boundaries.

## Core Interfaces

The top-level Data Service contracts belong in a Spring-free module.

```java
public interface DataServiceExecutor {
    String name();

    DataServiceCapabilities capabilities();
}
```

```java
public interface QueryExecutor extends DataServiceExecutor {
    QueryResult query(UserContext ctx, QueryRequest request);
}
```

```java
public interface MutationExecutor extends DataServiceExecutor {
    MutationResult mutate(UserContext ctx, MutationRequest request);
}
```

```java
public interface TransactionExecutor extends DataServiceExecutor {
    <T> T executeInTransaction(UserContext ctx, TransactionCallback<T> action);
}
```

Provider-specific implementations may implement only the interfaces they support.

For example:

- SQL JDBC provider: query, mutation, aggregation, schema, transaction, ID generation.
- Memory provider: query, mutation, aggregation, transaction by snapshot or no-op.
- Meilisearch provider: query/search and document mutation, but no relational transaction.
- Remote API provider: capability depends on the remote service.

## Capabilities

Capabilities must be explicit. The runtime must not assume that every provider supports SQL-style behavior.

```java
public final class DataServiceCapabilities {
    private boolean query;
    private boolean streamingQuery;
    private boolean mutation;
    private boolean batchMutation;
    private boolean aggregation;
    private boolean transaction;
    private boolean schema;
    private boolean idGeneration;
    private boolean relationLoad;
    private boolean relationMutation;
    private boolean fullTextSearch;
    private boolean returning;
}
```

Before executing an operation, the runtime checks the target provider capability and fails early with a clear error when unsupported.

This matters for providers such as Meilisearch. It can search and accept document updates, but it should not be treated as a relational SQL database with joins, foreign keys, or local ACID transactions.

## Execution Metadata

SQL logging should become a specialization of Data Service execution logging.

```java
public final class ExecutionMetadata {
    private String backend;
    private DataServiceOperation operation;
    private Instant startedAt;
    private Instant endedAt;
    private Long affectedRows;
    private Integer resultCount;
    private String backendRequestId;
    private String debugQuery;
    private List<TraceNode> traceChain;
    private String comment;
}
```

Examples:

- SQL provider: `debugQuery` can be SQL with bound parameters.
- Meilisearch provider: `debugQuery` can be HTTP method, URL, and JSON payload.
- Memory provider: `debugQuery` can describe the evaluated criteria.
- Remote provider: `backendRequestId` can carry a remote request ID.

`SqlLogEntry` may remain for compatibility, but the future logging layer should center on `DataServiceExecutionLog`.

## Routing

Java metadata needs a `dataService` concept equivalent to Rust's `data_service`.

```text
Order          -> sql
ProductSearch  -> meilisearch
AuditEvent     -> sql or append-log
CacheEntry     -> memory
```

The runtime flow should be:

```text
SearchRequest
  -> UserContext
  -> resolve EntityDescriptor
  -> read descriptor.dataService, defaulting to "sql"
  -> DataServiceRegistry.resolve(dataService)
  -> execute through the matching DataServiceExecutor
```

No application code should need to know whether a request is backed by Spring JDBC, plain JDBC, Android SQLite, memory, or Meilisearch.

## Registry

`DataServiceRegistry` is the explicit runtime entry point for provider lookup.

```java
public interface DataServiceRegistry {
    DataServiceExecutor resolve(String name);

    QueryExecutor resolveQueryExecutor(String name);

    MutationExecutor resolveMutationExecutor(String name);

    Optional<TransactionExecutor> resolveTransactionExecutor(String name);
}
```

The non-Spring runtime should use explicit registration:

```java
TeaQLRuntime runtime = TeaQLRuntime.builder()
    .metadata(entityMetaFactory)
    .dataService("sql", sqlDataService)
    .dataService("memory", memoryDataService)
    .requestPolicy(new PurposeRequestPolicy())
    .logManager(new LogManager())
    .build();

UserContext ctx = new UserContext(runtime);
```

Spring Boot can adapt this through beans, but the explicit runtime builder is the main architecture path.

## Transaction Model

Transactions are a provider capability.

The runtime must not pretend that all providers support ACID transactions.

```text
SQL Spring JDBC
  joins Spring @Transactional or opens TransactionTemplate

SQL plain JDBC
  uses Connection, autoCommit=false, and thread-bound transaction state

Android SQLite
  uses SQLiteDatabase.beginTransaction / setTransactionSuccessful / endTransaction

Memory
  can use snapshot, copy-on-write, or no-op semantics

Meilisearch
  usually does not support local relational transactions

Remote providers
  depend on remote backend semantics
```

Default nested transaction semantics should be `REQUIRED`:

```text
If current provider transaction exists:
  reuse it
else:
  open a provider transaction
```

`REQUIRES_NEW` and cross-provider transactions are not part of the first design.

Cross-provider mutation should be treated as eventual consistency, outbox, compensation, or explicit unsupported behavior. It should not be represented as a local ACID transaction.

## SQL Provider Stack

SQL is still important, but it should be a provider stack:

```text
SqlDataServiceExecutor
  -> SQL query compiler
  -> SQL mutation compiler
  -> SQL schema support
  -> SQL execution adapter
```

Execution adapters:

```text
SpringJdbcSqlExecutor
  -> DataSourceUtils
  -> TransactionTemplate
  -> Spring-managed transaction context

JdbcSqlExecutor
  -> DataSource / Connection / PreparedStatement
  -> direct JDBC transaction

AndroidSQLiteSqlExecutor
  -> SQLiteDatabase
  -> Android SQLite transaction
```

`TeaQLDatabase` should be downgraded to a low-level SQL execution interface used inside the SQL provider. It is not the runtime-level Data Service abstraction.

## Meilisearch Provider Shape

A Java Meilisearch provider should be possible without changing generated Q APIs.

```text
Q.products()
  -> SearchRequest
  -> descriptor.dataService = "meilisearch"
  -> MeilisearchDataServiceExecutor
  -> compile criteria to Meilisearch filter/search payload
  -> HTTP request
  -> map hits to Records / entities
```

Expected capabilities:

```text
query: true
fullTextSearch: true
mutation: true, for document indexing/update/delete
aggregation: limited or false, depending on supported facets/stats
transaction: false
schema: limited, for index/settings sync
relationLoad: false unless explicitly implemented
relationMutation: false
```

Unsupported operations should fail with a provider-specific, AI-actionable message:

```text
Provider "meilisearch" does not support relation mutation for entity ProductSearch.
Use the SQL data service for graph mutation, or publish search documents through an outbox/indexing flow.
```

## Module Direction

Target module layout:

```text
teaql
  Entity, SearchRequest, UserContext, RequestPolicy, base runtime contracts

teaql-data-service
  DataServiceExecutor, QueryExecutor, MutationExecutor,
  TransactionExecutor, SchemaExecutor, IdGeneratorExecutor,
  DataServiceCapabilities, ExecutionMetadata, DataServiceRegistry

teaql-data-service-sql
  SQL compiler, SQL metadata mapping, SQL Data Service executor
  no Spring dependency

teaql-provider-jdbc
  Direct JDBC execution and transaction adapter

teaql-provider-spring-jdbc
  Spring transaction-aware SQL execution adapter

teaql-provider-memory
  Memory Data Service executor

teaql-provider-meilisearch
  Future Java Meilisearch provider

teaql-provider-android-sqlite
  Optional Android SQLite execution adapter if the project decides to ship Android SDK integration

teaql-autoconfigure
  Spring Boot wiring adapter only
```

The existing `teaql-sql` can remain as a compatibility module during migration, but internally it should move toward `teaql-data-service-sql` plus `teaql-provider-spring-jdbc`.

## Relationship to Spring

Spring remains supported, but only as an adapter.

Spring Boot responsibilities:

- Discover or create Data Service executors.
- Register them into `DataServiceRegistry`.
- Create `UserContext` through `UserContextFactory`.
- Attach servlet or reactive request data.
- Adapt Spring transactions for the Spring JDBC provider.

Spring should not define the core runtime shape.

## Relationship to Generated Code

Generated Java code should not assume Spring annotations or Spring beans.

Generator targets should be explicit:

```text
java-runtime
  entities, requests, Q, E, checkers, metadata, no Spring annotations

java-spring-boot
  Spring controllers, @TQLContext integration, Spring configuration

java-data-service-sql
  SQL metadata and wiring hints for SQL providers
```

Generated request execution remains stable:

```java
Q.orders()
    .filterByMerchant(ctx.getMerchant())
    .comment("Load merchant orders")
    .purpose("Render order list")
    .executeForList(ctx);
```

The runtime decides which Data Service handles `Order`.

## Migration Plan

1. Add `teaql-data-service` with Rust-aligned contracts.
2. Add `DataServiceCapabilities`, `ExecutionMetadata`, and `DataServiceOperation`.
3. Add `DataServiceRegistry` and explicit `TeaQLRuntime` builder.
4. Add `dataService` to Java entity metadata, defaulting to `sql`.
5. Route query, aggregation, mutation, schema, and ID generation through Data Service executors.
6. Split SQL compilation from Spring JDBC execution.
7. Rebuild current Spring SQL path as `SqlDataServiceExecutor + SpringJdbcSqlExecutor`.
8. Rebuild portable SQL path as `SqlDataServiceExecutor + JdbcSqlExecutor` or Android SQLite adapter.
9. Keep current repository APIs as compatibility facades while they delegate to Data Service.
10. Add a no-Spring smoke test and a memory provider smoke test.
11. Add a future Meilisearch provider design or prototype after the routing model is stable.

## Non-Goals

This design does not require immediate removal of `TQLResolver`.

This design does not require immediate removal of `GLobalResolver`.

This design does not require a first-phase distributed transaction protocol.

This design does not require every provider to support every TeaQL operation.

This design does not require shipping an Android SDK module immediately.

## Final Target

TeaQL Java should have the same conceptual center as TeaQL Rust:

```text
Data Service provider architecture
```

The runtime routes generated business requests to named Data Services based on entity metadata and provider capabilities.

SQL, Spring JDBC, direct JDBC, Android SQLite, memory, Meilisearch, and future backends are implementation choices behind that boundary.


# teaql-java

TeaQL Java runtime: domain-driven data runtime for Java applications.
Works with or without Spring Boot. Android ready. Rust-aligned.

## Modules

- `teaql`: core entities, repositories, requests, criteria, metadata, audit logging.
- `teaql-sql`: SQL repository implementation (spring-jdbc).
- `teaql-android`: Android repository (no spring-jdbc, uses TeaQLDatabase abstraction).
- `teaql-autoconfigure`: Spring Boot auto configuration.
- `teaql-starter`: Spring Boot starter dependency.
- `teaql-sqlite`, `teaql-mysql`, `teaql-mssql`, `teaql-oracle`, `teaql-db2`,
  `teaql-hana`, `teaql-duck`, `teaql-snowflake`: database-specific repositories.
- `teaql-graphql`: GraphQL integration.

## Quick Start

### With Spring Boot

```xml
<dependency>
    <groupId>io.teaql</groupId>
    <artifactId>teaql-starter</artifactId>
</dependency>
```

### Without Spring Boot

```java
UserContext ctx = new UserContext();
ctx.setRequestPolicy(new PurposeRequestPolicy());
ctx.setLogManager(new LogManager());

Q.tasks()
    .comment("查询任务")
    .purpose("展示看板")
    .executeForList(ctx);
```

## Key Features

- **JPMS module boundaries**: internal packages sealed, only public API exported.
- **Compile-time query enforcement**: `purpose()` returns `ExecutableRequest`.
- **RequestPolicy**: pluggable operation-level policies (aligned with Rust).
- **Dual-layer audit logging**: runtime env vars + app-level customizable sinks.
- **Android support**: `teaql-android` module with `TeaQLDatabase` abstraction.

## SQLite

```properties
spring.datasource.url=jdbc:sqlite:./data/app.db
spring.datasource.driver-class-name=org.sqlite.JDBC
```

`teaql-sqlite` automatically wraps pooled data sources as `SingleConnectionDataSource`.

## Development

```bash
mvn clean compile
```

# teaql-spring-boot-starter

TeaQL Spring Boot starter provides Spring Boot integration and database-specific
repository implementations for TeaQL.

## Modules

- `teaql`: core TeaQL entities, repositories, requests, criteria, and metadata.
- `teaql-sql`: shared SQL repository implementation.
- `teaql-autoconfigure`: Spring Boot auto configuration.
- `teaql-sqlite`: SQLite repository support.
- `teaql-mysql`, `teaql-mssql`, `teaql-oracle`, `teaql-db2`, `teaql-hana`,
  `teaql-duck`, `teaql-snowflake`: database-specific repository support.

## SQLite

SQLite allows only limited concurrent writes. When TeaQL allocates a new entity
id it updates `teaql_id_space`, and then writes the entity tables. If a pooled
data source opens multiple SQLite connections, this can cause `SQLITE_BUSY`
errors or transaction failures.

`teaql-sqlite` automatically wraps SQLite JDBC data sources as a
`SingleConnectionDataSource` when the repository is created. This includes common
data sources that expose the JDBC URL through `getJdbcUrl()` or `getUrl()`, such
as HikariCP-style data sources.

Example:

```properties
spring.datasource.url=jdbc:sqlite:./data/app.db
spring.datasource.driver-class-name=org.sqlite.JDBC
```

With the SQLite repository, users no longer need to manually configure a
`NonClosingSingleConnectionDataSource` for the common pooled data source case.

### Transactions

The SQLite single-connection wrapper suppresses `Connection.close()` calls from
Spring transaction boundaries and closes the physical connection only when the
data source itself is closed. This allows a `@Transactional` operation to create
an entity successfully when the flow includes both:

- updating `teaql_id_space` for id allocation;
- inserting or updating the application entity tables.

The behavior is covered by `teaql-sqlite` tests.

## Usage

`UserContext` serves as the primary entry point for business logic. In a Spring MVC or WebFlux controller, you can directly declare `UserContext` as a parameter. It will be automatically resolved and initialized via the configured `UserContextFactory`:

```java
import io.teaql.data.UserContext;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class MyController {

    @GetMapping("/action")
    public Object action(UserContext ctx) {
        // execute query via UserContext
        return ctx.executeForList(request);
    }
}
```

An explicit `@TQLContext` annotation is also supported for backwards compatibility:

```java
import io.teaql.data.TQLContext;
import io.teaql.data.UserContext;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class MyController {

    @GetMapping("/action")
    public Object action(@TQLContext UserContext ctx) {
        return ctx.executeForList(request);
    }
}
```

## Development

Run the SQLite module tests:

```bash
gradle --no-daemon :teaql-sqlite:test
```

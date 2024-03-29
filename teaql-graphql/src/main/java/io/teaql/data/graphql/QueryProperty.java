package io.teaql.data.graphql;

import java.lang.annotation.*;

@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
public @interface QueryProperty {
  String value();
}

package io.teaql.data;

public interface GraphqlService {
  Object execute(UserContext ctx, String query);
}

package io.teaql.data;

public interface GraphQLService {
    Object execute(UserContext ctx, String query);
}

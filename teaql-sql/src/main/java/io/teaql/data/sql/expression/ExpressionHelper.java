package io.teaql.data.sql.expression;

import java.util.Map;

import io.teaql.data.Expression;
import io.teaql.data.RepositoryException;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLColumnResolver;
import io.teaql.data.sql.SQLRepository;

public class ExpressionHelper {

    public static String toSql(
            UserContext userContext,
            Expression expression,
            String idTable,
            Map<String, Object> parameters,
            SQLColumnResolver sqlColumnResolver) {
        return toSqlInternal(userContext, expression, idTable, parameters,
                sqlColumnResolver.getExpressionParsers(), sqlColumnResolver);
    }

    public static String toSql(
            UserContext userContext,
            Expression expression,
            String idTable,
            Map<String, Object> parameters,
            Map<Class, SQLExpressionParser> parsers,
            SQLColumnResolver columnResolver) {
        return toSqlInternal(userContext, expression, idTable, parameters, parsers, columnResolver);
    }

    private static String toSqlInternal(
            UserContext userContext,
            Expression expression,
            String idTable,
            Map<String, Object> parameters,
            Map<Class, SQLExpressionParser> parsers,
            SQLColumnResolver columnResolver) {
        if (expression == null) {
            return null;
        }
        if (expression instanceof SQLExpressionParser) {
            return ((SQLExpressionParser) expression)
                    .toSql(userContext, expression, idTable, parameters, columnResolver);
        }

        Class expressionClass = expression.getClass();
        SQLExpressionParser parser = null;

        while (expressionClass != null) {
            parser = parsers.get(expressionClass);
            if (parser != null) {
                break;
            }
            expressionClass = expressionClass.getSuperclass();
        }
        if (parser == null) {
            throw new RepositoryException("no parse for expression type:" + expression.getClass());
        }
        return parser.toSql(userContext, expression, idTable, parameters, columnResolver);
    }
}

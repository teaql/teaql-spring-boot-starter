package io.teaql.data.sql.expression;

import java.util.Map;

import io.teaql.data.SearchCriteria;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.VersionSearchCriteria;
import io.teaql.data.sql.SQLRepository;

public class VersionSearchCriteriaParser implements SQLExpressionParser<VersionSearchCriteria> {
    public Class<VersionSearchCriteria> type() {
        return VersionSearchCriteria.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            VersionSearchCriteria expression,
            String idTable,
            Map<String, Object> parameters,
            SQLRepository sqlColumnResolver) {
        SearchCriteria searchCriteria = expression.getSearchCriteria();
        return ExpressionHelper.toSql(
                userContext, searchCriteria, idTable, parameters, sqlColumnResolver);
    }
}

package io.teaql.data.sql.expression;

import java.util.Map;

import cn.hutool.core.util.StrUtil;

import io.teaql.data.Parameter;
import io.teaql.data.SearchCriteria;
import io.teaql.data.TypeCriteria;
import io.teaql.data.UserContext;
import io.teaql.data.sql.SQLColumn;
import io.teaql.data.sql.SQLRepository;

public class TypeCriteriaParser implements SQLExpressionParser<TypeCriteria> {
    @Override
    public Class<TypeCriteria> type() {
        return TypeCriteria.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            TypeCriteria expression,
            String idTable,
            Map<String, Object> parameters,
            SQLRepository sqlColumnResolver) {
        SQLColumn childType = sqlColumnResolver.getPropertyColumn(idTable, "_child_type");
        if (childType == null) {
            return SearchCriteria.TRUE;
        }
        Parameter typeParameter = expression.getTypeParameter();
        String parameterSql =
                ExpressionHelper.toSql(userContext, typeParameter, idTable, parameters, sqlColumnResolver);

        if (userContext.getBool(SQLRepository.MULTI_TABLE, false)) {
            return StrUtil.format("{}._child_type in ({})", childType.getTableName(), parameterSql);
        }
        return StrUtil.format("_child_type in ({})", parameterSql);
    }
}

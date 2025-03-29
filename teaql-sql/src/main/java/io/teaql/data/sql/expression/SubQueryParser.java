package io.teaql.data.sql.expression;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import cn.hutool.core.util.ObjectUtil;

import io.teaql.data.Entity;
import io.teaql.data.Parameter;
import io.teaql.data.PropertyReference;
import io.teaql.data.Repository;
import io.teaql.data.SearchCriteria;
import io.teaql.data.SearchRequest;
import io.teaql.data.SmartList;
import io.teaql.data.SubQuerySearchCriteria;
import io.teaql.data.TempRequest;
import io.teaql.data.UserContext;
import io.teaql.data.criteria.IN;
import io.teaql.data.criteria.InLarge;
import io.teaql.data.criteria.Operator;
import io.teaql.data.criteria.RawSql;
import io.teaql.data.sql.SQLColumnResolver;
import io.teaql.data.sql.SQLRepository;

public class SubQueryParser implements SQLExpressionParser<SubQuerySearchCriteria> {

    public static final String IGNORE_SUBTYPES = "IGNORE_SUBTYPES";

    @Override
    public Class<SubQuerySearchCriteria> type() {
        return SubQuerySearchCriteria.class;
    }

    @Override
    public String toSql(
            UserContext userContext,
            SubQuerySearchCriteria expression,
            String idTable,
            Map<String, Object> parameters,
            SQLRepository sqlColumnResolver) {
        SearchRequest dependsOn = expression.getDependsOn();
        String propertyName = expression.getPropertyName();
        String dependsOnPropertyName = expression.getDependsOnPropertyName();
        String type = dependsOn.getTypeName();
        Repository repository = userContext.resolveRepository(type);

        if (dependsOn.tryUseSubQuery()
                && isRequestInDatasource(userContext, sqlColumnResolver, repository)) {
            SQLRepository subRepository = (SQLRepository) repository;
            TempRequest tempRequest = new TempRequest(dependsOn.returnType(), dependsOn.getTypeName());

            tempRequest.setOrderBy(dependsOn.getOrderBy());

            tempRequest.setSlice(dependsOn.getSlice());

            // select depends on property
            tempRequest.selectProperty(dependsOnPropertyName);
            tempRequest.appendSearchCriteria(dependsOn.getSearchCriteria());

            userContext.put(IGNORE_SUBTYPES, true);
            String subQuery = subRepository.buildDataSQL(userContext, tempRequest, parameters);
            userContext.del(IGNORE_SUBTYPES);
            if (ObjectUtil.isEmpty(subQuery)) {
                return SearchCriteria.FALSE;
            }
            IN in = new IN(new PropertyReference(propertyName), new RawSql(subQuery));
            return ExpressionHelper.toSql(userContext, in, idTable, parameters, sqlColumnResolver);
        }

        // fall back
        SmartList<Entity> referred = repository.executeForList(userContext, dependsOn);
        Set dependsOnValues = new HashSet<>();
        for (Entity entity : referred) {
            Object propertyValue = entity.getProperty(dependsOnPropertyName);
            if (!ObjectUtil.isEmpty(propertyValue)) {
                dependsOnValues.add(propertyValue);
            }
        }
        Parameter parameter = new Parameter(propertyName, dependsOnValues, Operator.IN_LARGE);
        InLarge in = new InLarge(new PropertyReference(propertyName), parameter);
        return ExpressionHelper.toSql(userContext, in, idTable, parameters, sqlColumnResolver);
    }

    private boolean isRequestInDatasource(
            UserContext pUserContext, SQLColumnResolver pSqlColumnResolver, Repository pRepository) {
        if (!(pSqlColumnResolver instanceof SQLRepository)) {
            return false;
        }
        return ((SQLRepository<?>) pSqlColumnResolver).isRequestInDatasource(pUserContext, pRepository);
    }
}

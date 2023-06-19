package io.teaql.data.sql.expression;

import cn.hutool.core.util.ObjectUtil;
import io.teaql.data.*;
import io.teaql.data.criteria.IN;
import io.teaql.data.criteria.RawSql;
import io.teaql.data.sql.SQLColumnResolver;
import io.teaql.data.sql.SQLRepository;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class SubQueryParser implements SQLExpressionParser<SubQuerySearchCriteria> {
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

      tempRequest.setSlice(dependsOn.getSlice());

      // 只选择依赖的属性以及条件
      tempRequest.selectProperty(dependsOnPropertyName);
      tempRequest.appendSearchCriteria(dependsOn.getSearchCriteria());
      String subQuery = subRepository.buildDataSQL(userContext, tempRequest, parameters);
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
    Parameter parameter = new Parameter(propertyName, dependsOnValues);
    IN in = new IN(new PropertyReference(propertyName), parameter);
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

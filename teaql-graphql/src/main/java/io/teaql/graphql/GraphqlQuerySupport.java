package io.teaql.graphql;

import cn.hutool.core.util.ClassUtil;
import io.teaql.data.BaseRequest;
import io.teaql.data.UserContext;
import io.teaql.data.meta.EntityDescriptor;
import io.teaql.data.meta.PropertyDescriptor;

public interface GraphqlQuerySupport {

  // indicate to the search request for this object field(this field is object),
  // if getRequestProperty return not null, then the request will add parent level(source items)
  // criteria
  default BaseRequest buildQuery(GraphqlFetcherParam param) {
    GraphqlFieldQuery fieldQuery = findFieldQuery(param);
    return fieldQuery.buildQuery(param.userContext(), param.params());
  }

  // the request property linked to the parent(source) property,
  // then parent request will select this property and pass thought to build this field search
  // request, the property name should be the property from parent to this field
  default String getRequestProperty(GraphqlFetcherParam param) {
    UserContext userContext = param.userContext();
    String parentType = param.parentType();
    EntityDescriptor entityDescriptor = userContext.resolveEntityDescriptor(parentType);
    PropertyDescriptor property = entityDescriptor.findProperty(param.field());
    // simple fields
    if (property != null && ClassUtil.isSimpleValueType(property.getType().javaType())) {
      return param.field();
    }
    return findFieldQuery(param).getRequestProperty();
  }

  GraphqlFieldQuery findFieldQuery(GraphqlFetcherParam param);

  void register(GraphqlFieldQuery query);
}

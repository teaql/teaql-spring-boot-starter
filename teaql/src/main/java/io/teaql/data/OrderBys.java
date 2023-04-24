package io.teaql.data;

import java.util.ArrayList;
import java.util.List;

public class OrderBys implements Expression {
  private List<OrderBy> orderBys = new ArrayList<>();

  public List<OrderBy> getOrderBys() {
    return orderBys;
  }

  public void setOrderBys(List<OrderBy> pOrderBys) {
    orderBys = pOrderBys;
  }

  public OrderBys addOrderBy(OrderBy orderBy) {
    if (orderBy != null) {
      orderBys.add(orderBy);
    }
    return this;
  }

  @Override
  public List<String> properties(UserContext ctx) {
    List<String> ret = new ArrayList<>();

    for (Expression expression : orderBys) {
      List<String> properties = expression.properties(ctx);
      if (properties != null) {
        ret.addAll(properties);
      }
    }
    return ret;
  }

  public boolean isEmpty() {
    return orderBys.isEmpty();
  }
}

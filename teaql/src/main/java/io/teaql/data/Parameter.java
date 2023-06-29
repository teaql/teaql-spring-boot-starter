package io.teaql.data;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.ObjectUtil;
import io.teaql.data.criteria.Operator;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Parameter implements Expression {
  private String name;
  private Object value;
  private Operator operator;

  public Parameter(String name, Object value, Operator operator) {
    this.name = name;
    this.operator = operator;
    List values = flatValues(value);
    if (operator.hasMultiValue()) {
      this.value = values;
    } else {
      Object first = CollectionUtil.getFirst(values);
      this.value = first;
    }
  }

  public Parameter(String name, Object value, boolean multiValue) {
    this.name = name;
    List values = flatValues(value);
    if (multiValue) {
      this.value = values.toArray();
    } else {
      Object first = CollectionUtil.getFirst(values);
      this.value = first;
    }
  }

  public Parameter(String name, Object value) {
    this(name, value, true);
  }

  public Object getValue() {
    return value;
  }

  public String getName() {
    return name;
  }

  private List flatValues(Object value) {
    List ret = new ArrayList();
    visit(ret, value);
    return ret;
  }

  private void visit(List ret, Object pValue) {
    if (ObjectUtil.isEmpty(pValue)) {
      return;
    }
    if (ArrayUtil.isArray(pValue)) {
      int length = ArrayUtil.length(pValue);
      for (int i = 0; i < length; i++) {
        visit(ret, ArrayUtil.get(pValue, i));
      }
    } else if (pValue instanceof Iterator) {
      Iterator it = (Iterator) pValue;
      while (it.hasNext()) {
        visit(ret, it.next());
      }
    } else if (pValue instanceof Iterable) {
      visit(ret, ((Iterable<?>) pValue).iterator());
    } else if (pValue instanceof Entity) {
      ret.add(((Entity) pValue).getId());
    } else {
      ret.add(pValue);
    }
  }

  public Operator getOperator() {
    return operator;
  }

  public void setOperator(Operator pOperator) {
    operator = pOperator;
  }
}

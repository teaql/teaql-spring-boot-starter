package io.teaql.data;

import java.util.Collections;
import java.util.List;

/**
 * @author Jackytin
 *     <p>描述与某一组属性相关
 */
public interface PropertyAware {

  default List<String> properties(UserContext ctx) {
    return Collections.emptyList();
  }

}

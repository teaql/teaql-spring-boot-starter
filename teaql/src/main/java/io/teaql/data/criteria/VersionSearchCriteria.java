package io.teaql.data.criteria;

import io.teaql.data.SearchCriteria;
import io.teaql.data.UserContext;
import java.util.List;

public class VersionSearchCriteria implements SearchCriteria {
  private SearchCriteria searchCriteria;

  public VersionSearchCriteria(SearchCriteria pSearchCriteria) {
    searchCriteria = pSearchCriteria;
  }

  @Override
  public List<String> properties(UserContext ctx) {
    return searchCriteria.properties(ctx);
  }

  public SearchCriteria getSearchCriteria() {
    return searchCriteria;
  }

  public void setSearchCriteria(SearchCriteria pSearchCriteria) {
    searchCriteria = pSearchCriteria;
  }
}

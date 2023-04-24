package io.teaql.data;

public class SimpleAggregation implements Expression {
  private String name;
  private SearchRequest aggregateRequest;

  public SimpleAggregation(String name, SearchRequest pAggregateRequest) {
    this.name = name;
    aggregateRequest = pAggregateRequest;
  }

  public String getName() {
    return name;
  }

  public void setName(String pName) {
    name = pName;
  }

  public SearchRequest getAggregateRequest() {
    return aggregateRequest;
  }

  public void setAggregateRequest(SearchRequest pAggregateRequest) {
    aggregateRequest = pAggregateRequest;
  }
}

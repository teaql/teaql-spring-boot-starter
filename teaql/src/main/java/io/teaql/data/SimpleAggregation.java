package io.teaql.data;

public class SimpleAggregation implements Expression {
  private String name;
  private SearchRequest aggregateRequest;

  private boolean singleNumber;

  public SimpleAggregation(String name, SearchRequest pAggregateRequest) {
    this.name = name;
    aggregateRequest = pAggregateRequest;
  }

  public SimpleAggregation(String name, SearchRequest aggregateRequest, boolean singleNumber) {
    this.name = name;
    this.aggregateRequest = aggregateRequest;
    this.singleNumber = singleNumber;
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

  public boolean isSingleNumber() {
    return singleNumber;
  }

  public void setSingleNumber(boolean pSingleNumber) {
    singleNumber = pSingleNumber;
  }
}

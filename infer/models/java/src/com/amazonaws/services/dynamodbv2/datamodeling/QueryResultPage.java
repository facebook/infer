package com.amazonaws.services.dynamodbv2.datamodeling;
import java.util.Map;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
public class QueryResultPage<T> {
   public QueryResultPage () {}
   public Map<String,AttributeValue> getLastEvaluatedKey() {
      return null;
   }
}

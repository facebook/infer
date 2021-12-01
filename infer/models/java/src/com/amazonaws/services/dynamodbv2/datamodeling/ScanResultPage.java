package com.amazonaws.services.dynamodbv2.datamodeling;
import java.util.Map;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
public class ScanResultPage<T> {
   public ScanResultPage () {}
   public Map<String,AttributeValue> getLastEvaluatedKey() {
      return null;
   }
}

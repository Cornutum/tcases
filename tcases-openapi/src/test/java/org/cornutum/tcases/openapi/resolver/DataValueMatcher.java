//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.cornutum.tcases.resolve.DataValue;
import org.hamcrest.Matchers;

import static org.cornutum.tcases.resolve.DataValue.Type.*;

import java.util.List;
import java.util.Map;

/**
 * A composite matcher for {@link DataValue} objects.
 */
@SuppressWarnings("rawtypes")
public class DataValueMatcher extends BaseCompositeMatcher<DataValue>
  {
  /**
   * Creates a new DataValueMatcher instance.
   */
  public DataValueMatcher( DataValue expected)
    {
    super( expected);

    expectThat( valueOf( "type", DataValue::getType).matches( Matchers::equalTo));
    expectThat( valueOf( "array", this::getArrayData).matches( listsMembersMatching( DataValueMatcher::new)));
    expectThat( valueOf( "boolean", this::getBooleanData).matches( Matchers::equalTo));
    expectThat( valueOf( "integer", this::getIntegerData).matches( Matchers::equalTo));
    expectThat( valueOf( "null", this::getNullData).matches( Matchers::equalTo));
    expectThat( valueOf( "number", this::getNumberData).matches( Matchers::equalTo));
    expectThat( valueOf( "object", this::getObjectData).matches( ObjectDataMatcher::new));
    expectThat( valueOf( "string", this::getStringData).matches( Matchers::equalTo));
    expectThat( valueOf( "binary", this::getBinaryData).matches( Matchers::equalTo));
    expectThat( valueOf( "format", DataValue::getFormat).matches( Matchers::equalTo));
    }

  @SuppressWarnings("unchecked")
  private List<DataValue> getArrayData( DataValue dataValue)
    {
    return
      dataValue.getType().equals( ARRAY)
      ? (List<DataValue>) dataValue.getValue()
      : null;
    }

  private Boolean getBooleanData( DataValue dataValue)
    {
    return
      dataValue.getType().equals( BOOLEAN)
      ? (Boolean) dataValue.getValue()
      : null;
    }

  private Number getIntegerData( DataValue dataValue)
    {
    return
      dataValue.getType().equals( INTEGER)
      ? (Number) dataValue.getValue()
      : null;
    }

  private Object getNullData( DataValue dataValue)
    {
    return
      dataValue.getType().equals( NULL)
      ? dataValue.getValue()
      : "?";
    }

  private Number getNumberData( DataValue dataValue)
    {
    return
      dataValue.getType().equals( NUMBER)
      ? (Number) dataValue.getValue()
      : null;
    }

  @SuppressWarnings("unchecked")
  private Map<String,DataValue<?>> getObjectData( DataValue dataValue)
    {
    return
      dataValue.getType().equals( OBJECT)
      ? (Map<String,DataValue<?>>) dataValue.getValue()
      : null;
    }

  private String getStringData( DataValue dataValue)
    {
    return
      dataValue.getType().equals( STRING) && !"binary".equals( dataValue.getFormat())
      ? (String) dataValue.getValue()
      : null;
    }

  private byte[] getBinaryData( DataValue dataValue)
    {
    return
      dataValue.getType().equals( STRING) && "binary".equals( dataValue.getFormat())
      ? (byte[]) dataValue.getValue()
      : null;
    }
  }

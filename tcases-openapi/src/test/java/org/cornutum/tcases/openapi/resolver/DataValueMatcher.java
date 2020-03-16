//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.hamcrest.BaseCompositeMatcher;

import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link DataValue} objects.
 */
public class DataValueMatcher extends BaseCompositeMatcher<DataValue<?>>
  {
  /**
   * Creates a new DataValueMatcher instance.
   */
  public DataValueMatcher( DataValue<?> expected)
    {
    super( expected);

    expectThat( valueOf( "value", this::getDataObject).matches( Matchers::equalTo));
    expectThat( valueOf( "type", DataValue::getType).matches( Matchers::equalTo));
    expectThat( valueOf( "format", DataValue::getFormat).matches( Matchers::equalTo));
    }

  private Object getDataObject( DataValue<?> dataValue)
    {
    return (Object) dataValue.getValue();
    }

  }

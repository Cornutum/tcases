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

/**
 * Base class for {@link MessageData} matchers.
 */
public abstract class AbstractMessageDataMatcher<T extends MessageData> extends BaseCompositeMatcher<T>
  {
  /**
   * Creates a new AbstractMessageDataMatcher instance.
   */
  protected AbstractMessageDataMatcher( T expected)
    {
    super( expected);

    expectThat( valueOf( "value", this::getDataValue).matches( DataValueMatcher::new));
    expectThat( valueOf( "mediaType", MessageData::getMediaType).matches( Matchers::equalTo));
    expectThat( valueOf( "valid", MessageData::isValid).matches( Matchers::equalTo));
    expectThat( valueOf( "encodings", MessageData::getEncodings).matches( containsEntriesMatching( EncodingDataMatcher::new)));
    }

  @SuppressWarnings("rawtypes")
  private DataValue getDataValue( T messageData)
    {
    return messageData.getValue();
    }
  }

//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link EncodingData} objects.
 */
public class EncodingDataMatcher extends BaseCompositeMatcher<EncodingData>
  {
  /**
   * Creates a new EncodingDataMatcher instance.
   */
  public EncodingDataMatcher( EncodingData expected)
    {
    super( expected);

    expectThat( valueOf( "style", EncodingData::getStyle).matches( Matchers::equalTo));
    expectThat( valueOf( "exploded", EncodingData::isExploded).matches( Matchers::equalTo));
    expectThat( valueOf( "contentType", EncodingData::getContentType).matches( Matchers::equalTo));
    expectThat( valueOf( "headers", EncodingData::getHeaders).matches( ( containsMembersMatching( HeaderDataMatcher::new))));
    }
  }

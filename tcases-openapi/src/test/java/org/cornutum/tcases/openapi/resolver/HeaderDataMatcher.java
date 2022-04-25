//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link HeaderData} objects.
 */
public class HeaderDataMatcher extends AbstractMessageDataMatcher<HeaderData>
  {
  /**
   * Creates a new HeaderDataMatcher instance.
   */
  public HeaderDataMatcher( HeaderData expected)
    {
    super( expected);

    expectThat( valueOf( "name", HeaderData::getName).matches( Matchers::equalTo));
    expectThat( valueOf( "exploded", HeaderData::isExploded).matches( Matchers::equalTo));
    }
  }

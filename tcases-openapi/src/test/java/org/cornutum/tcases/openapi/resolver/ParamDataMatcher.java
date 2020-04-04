//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link ParamData} objects.
 */
public class ParamDataMatcher extends AbstractMessageDataMatcher<ParamData>
  {
  /**
   * Creates a new ParamDataMatcher instance.
   */
  public ParamDataMatcher( ParamData expected)
    {
    super( expected);

    expectThat( valueOf( "name", ParamData::getName).matches( Matchers::equalTo));
    expectThat( valueOf( "location", ParamData::getLocation).matches( Matchers::equalTo));
    expectThat( valueOf( "style", ParamData::getStyle).matches( Matchers::equalTo));
    expectThat( valueOf( "exploded", ParamData::isExploded).matches( Matchers::equalTo));
    }
  }

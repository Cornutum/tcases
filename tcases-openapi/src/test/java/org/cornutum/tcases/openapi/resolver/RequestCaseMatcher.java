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
 * A composite matcher for {@link RequestCase} objects.
 */
public class RequestCaseMatcher extends BaseCompositeMatcher<RequestCase>
  {
  /**
   * Creates a new RequestCaseMatcher instance.
   */
  public RequestCaseMatcher( RequestCase expected)
    {
    super( expected);

    expectThat( valueOf( "id", RequestCase::getId).matches( Matchers::equalTo));
    expectThat( valueOf( "name", RequestCase::getName).matches( Matchers::equalTo));
    expectThat( valueOf( "server", RequestCase::getServer).matches( Matchers::equalTo));
    expectThat( valueOf( "version", RequestCase::getVersion).matches( Matchers::equalTo));
    expectThat( valueOf( "api", RequestCase::getApi).matches( Matchers::equalTo));
    expectThat( valueOf( "path", RequestCase::getPath).matches( Matchers::equalTo));
    expectThat( valueOf( "operation", RequestCase::getOperation).matches( Matchers::equalTo));
    expectThat( valueOf( "params", RequestCase::getParams).matches( containsMembersMatching( ParamDataMatcher::new)));
    expectThat( valueOf( "body", RequestCase::getBody).matches( MessageDataMatcher::new));
    expectThat( valueOf( "invalidInput", RequestCase::getInvalidInput).matches( Matchers::equalTo));
    expectThat( valueOf( "auth", RequestCase::getAuthDefs).matches( containsMembersMatching( AuthDefMatcher::new)));
    expectThat( valueOf( "authFailure", RequestCase::isAuthFailure).matches( Matchers::equalTo));
    }
  }

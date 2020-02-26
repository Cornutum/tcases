//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.openapi.resolver.DataValue.Type;

import org.junit.Test;

/**
 * Runs tests for {@link MultiTypeDomain}.
 */
public class MultiTypeDomainTest extends ValueDomainTest
  {
  @Test
  public void whenAllTypes()
    {
    // Given...
    MultiTypeDomain domain = new MultiTypeDomain( Type.any());

    // Then...
    verifyContainsValues( domain, 1000);
    }
  }

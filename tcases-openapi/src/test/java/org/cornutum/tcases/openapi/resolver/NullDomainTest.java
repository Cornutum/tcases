//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.List;

/**
 * Runs tests for {@link NullDomain}.
 */
public class NullDomainTest extends ValueDomainTest
  {
  @Test
  public void whenNullDomain()
    {
    // Given...
    NullDomain domain = new NullDomain();

    // Then...
    List<Object> values = valuesOf( domain, 10);
    assertThat( "Null values size", values.size(), is( 1));
    assertThat( "Null value", domain.select( getResolverContext()), matches( dataValueMatcher( null, Type.NULL, null)));

    assertThat( "Contains", domain.contains( null), is( true));
    assertThat( "Contains", domain.contains( 1234), is( false));
    assertThat( "Contains", domain.contains( ""), is( false));
    }
    
  }

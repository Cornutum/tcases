//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.List;
import static java.util.stream.Collectors.toList;

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
    List<Object> values = domain.values( getRandom()).limit( 10).collect( toList());
    assertThat( "Null values size", values.size(), is( 1));
    assertThat( "Null value", domain.select( getRandom()), nullValue());

    assertThat( "Contains", domain.contains( null), is( true));
    assertThat( "Contains", domain.contains( 1234), is( false));
    assertThat( "Contains", domain.contains( ""), is( false));
    }
    
  }

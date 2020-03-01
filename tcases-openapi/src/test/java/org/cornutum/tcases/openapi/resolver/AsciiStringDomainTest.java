//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

import org.cornutum.tcases.VarBindingBuilder;
import org.cornutum.tcases.openapi.resolver.NumberDomain.Range;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;

import java.util.List;

/**
 * Runs tests for {@link AsciiStringDomain}.
 */
public class AsciiStringDomainTest extends ValueDomainTest
  {  
  @Test
  public void whenConstant()
    {
    // Given...
    String value = "Hello, world!";
    StringConstant domain = new StringConstant( value, "myformat");

    // Then...
    List<String> values = valuesOf( domain, 10);
    assertThat( "Constant values size", values.size(), is( 1));
    assertThat( "Constant value", domain.select( getResolverContext()), matches( dataValueMatcher( value, Type.STRING, "myformat")));
    assertThat( "Contains", domain.contains( value), is( true));
    assertThat( "Contains", domain.contains( ""), is( false));
    }

  @Test
  public void whenLengthAboveMin()
    {
    // Given...
    AsciiStringDomain domain = new AsciiStringDomain( 32);

    // When...
    domain.setLengthRange( Range.of( VarBindingBuilder.with( "Length").value( "> 16").build()));
    domain.setExcluded( Stream.of( "abcdefghijklmnopqrstuvwzyz").collect( toSet()));
    //   MatchPatterns = Some
    //   NotMatchPatterns = Some

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( "abcdefghijklmnopqrstuvwzyz"), is( false));
    assertThat( "Contains", domain.contains( "ABCDEFGHIJKLMNOPQRSTUVWZYZ"), is( true));
    assertThat( "Contains", domain.contains( "ABCDEFGHIJKLMNOP"), is( false));
    assertThat( "Contains", domain.contains( "ABCDEFGHIJKLMNOPQRSTUVWZYZABCDEFG"), is( false));
    assertThat( "Contains", domain.contains( "Mañana, Schrödinger"), is( false));

    // When...
    DataValue<String> value = domain.select( getResolverContext());

    // Then...
    assertThat( "Value", value, matches( dataValueMatcher( value.getValue(), Type.STRING, null)));
    }

  @Test
  public void whenLengthBelowMax()
    {
    // Given...
    AsciiStringDomain domain = new AsciiStringDomain( 32);

    // When...
    domain.setLengthRange( Range.of( VarBindingBuilder.with( "Length").value( "< 4").build()));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( "ABC"), is( true));
    assertThat( "Contains", domain.contains( ""), is( true));
    assertThat( "Contains", domain.contains( "ABCD"), is( false));
    }

  @Test
  public void whenLengthConstant()
    {
    // Given...
    AsciiStringDomain domain = new AsciiStringDomain( 32);

    // When...
    domain.setLengthRange( 8);
    domain.setExcluded( Stream.of( "01234567", "abcdefgh").collect( toSet()));
    //   MatchPatterns = Some
    //   NotMatchPatterns = None

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( "Abcdefgh"), is( true));
    assertThat( "Contains", domain.contains( ""), is( false));
    assertThat( "Contains", domain.contains( "01234567"), is( false));
    assertThat( "Contains", domain.contains( "abcdefgh"), is( false));
    assertThat( "Contains", domain.contains( "Abcdefghi"), is( false));
    assertThat( "Contains", domain.contains( "Abcdefg"), is( false));
    }  
  }

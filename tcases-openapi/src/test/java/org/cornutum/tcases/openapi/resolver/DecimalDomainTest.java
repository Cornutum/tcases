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
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.cornutum.tcases.openapi.resolver.DataValue.Type;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.stream.Stream;
import static java.util.stream.Collectors.toSet;

import java.math.BigDecimal;
import java.util.List;

/**
 * Runs tests for {@link DecimalDomain}.
 */
public class DecimalDomainTest extends ValueDomainTest
  {  
  @Test
  public void whenNotMultipleOf()
    {
    // Given...
    DecimalDomain domain = new DecimalDomain( "float");

    // When...
    domain.setRange( new BigDecimal( "7.0"), new BigDecimal( "10.0"));
    domain.setMultipleOf( "2");
    domain.setNotMultipleOfs( new String[]{ "5" });

    // Then...
    List<BigDecimal> values = valuesOf( domain, 10);
    assertThat( "Values size", values.size(), is( 1));
    assertThat( "Value", domain.select( getRandom()), matches( dataValueMatcher( new BigDecimal( "8.0"), Type.NUMBER, "float")));

    // When...
    domain.setRange( new BigDecimal( "9.0"), new BigDecimal( "11.0"));
    
    // Then...
    values = valuesOf( domain, 10);
    assertThat( "Values size", values.size(), is( 0));
    expectFailure( IllegalStateException.class).when( () -> domain.select( getRandom()));

    // When...
    domain.setRange( new BigDecimal( "-11.0"), new BigDecimal( "11.0"));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( new BigDecimal( "8.0")), is( true));
    assertThat( "Contains", domain.contains( new BigDecimal( "-12.0")), is( false));
    assertThat( "Contains", domain.contains( new BigDecimal( "10.0")), is( false));
    }
  
  @Test
  public void whenConstant()
    {
    // Given...
    DecimalConstant domain = new DecimalConstant( new BigDecimal( "-123456789.0"), "double");

    // Then...
    List<BigDecimal> values = valuesOf( domain, 10);
    assertThat( "Constant values size", values.size(), is( 1));
    assertThat( "Constant value", domain.select( getRandom()), matches( dataValueMatcher( new BigDecimal( "-123456789.0"), Type.NUMBER, "double")));
    }
  
  @Test
  public void whenNotMultipleOfs()
    {
    // Given...
    DecimalDomain domain = new DecimalDomain();

    // When...
    domain.setRange( Range.of( VarBindingBuilder.with( "Decimal").value( "< -999999999").build()));
    domain.setNotMultipleOfs( Stream.of( new BigDecimal( "5.0"), new BigDecimal( "2.0")).collect( toSet()));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( new BigDecimal( "-1000000001")), is( true));
    assertThat( "Contains", domain.contains( new BigDecimal( "-999999999")), is( false));
    assertThat( "Contains", domain.contains( new BigDecimal( "-1000000002")), is( false));
    }
  
  @Test
  public void whenMultipleOf()
    {
    // Given...
    DecimalDomain domain = new DecimalDomain();

    // When...
    domain.setRange( Range.of( VarBindingBuilder.with( "Decimal").value( "> 999999999").build()));
    domain.setMultipleOf( new BigDecimal( "2.125"));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( new BigDecimal( "1000000003.625")), is( true));
    assertThat( "Contains", domain.contains( new BigDecimal( "999999997.250")), is( false));
    assertThat( "Contains", domain.contains( new BigDecimal( "1000000000.000")), is( false));
    }

  @Test
  public void whenExcluded()
    {
    // Given...
    DecimalDomain domain = new DecimalDomain();

    // When...
    domain.setRange(
      Range.of(
        VarBindingBuilder.with( "Decimal")
        .value( "Other")
        .has( "excluded", "-1.23", "4.5", "67.89").build()));

    // Then...
    verifyContainsValues( domain, 1000);
    assertThat( "Contains", domain.contains( new BigDecimal( "4.5")), is( false));
    assertThat( "Contains", domain.contains( new BigDecimal( "890.12")), is( true));
    assertThat( "Contains", domain.contains( new BigDecimal( "0.00")), is( true));
    }
  }

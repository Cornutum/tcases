//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.VarValueDef.Type.*;
import static org.cornutum.tcases.conditions.Conditions.*;
import static org.cornutum.tcases.util.CollectionUtils.*;

import org.junit.Test;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import static java.util.stream.Collectors.*;

/**
 * Runs tests for generating test cases with bounded assertions.
 *
 */
public class TestBoundedAssertions
  {
  @Test
  public void withMinMaxConditions()
    {
    // Given...
    SystemInputDef inputDef =
      SystemInputDefBuilder.with( "Schemas")
      .functions(
        FunctionInputDefBuilder.with( "Object")
        .vars(
          "arg",

          VarDefBuilder.with( "Property-Count")
          .values(
            VarValueDefBuilder.with( "< 2")
            .when( lessThan( "propertyDefined", 2))
            .type( FAILURE) 
            .build(),
            VarValueDefBuilder.with( "<= 3")
            .when( between( "propertyDefined", 2, 3))
            .build(),
            VarValueDefBuilder.with( "> 3")
            .when( moreThan( "propertyDefined", 3))
            .type( FAILURE) 
            .build())
          .build(),

          VarSetBuilder.with( "Properties")
          .members(
            VarSetBuilder.with( "myString")
            .members(
              VarDefBuilder.with( "Defined")
              .values(
                VarValueDefBuilder.with( "Yes").properties( "propertyDefined").build(),
                VarValueDefBuilder.with( "No").build())
              .build())
            .build(),

            VarSetBuilder.with( "myArray")
            .members(
              VarDefBuilder.with( "Defined")
              .values(
                VarValueDefBuilder.with( "Yes").properties( "propertyDefined").build(),
                VarValueDefBuilder.with( "No").build())
              .build())
            .build(),

            VarSetBuilder.with( "myNumber")
            .members(
              VarDefBuilder.with( "Defined")
              .values(
                VarValueDefBuilder.with( "Yes").properties( "propertyDefined").build(),
                VarValueDefBuilder.with( "No").build())
              .build())
            .build(),
          
            VarSetBuilder.with( "myInteger")
            .members(
              VarDefBuilder.with( "Defined")
              .values(
                VarValueDefBuilder.with( "Yes").properties( "propertyDefined").build(),
                VarValueDefBuilder.with( "No").build())
              .build())
            .build(),
          
            VarSetBuilder.with( "myBoolean")
            .members(
              VarDefBuilder.with( "Defined")
              .values(
                VarValueDefBuilder.with( "Yes").properties( "propertyDefined").build(),
                VarValueDefBuilder.with( "No").build())
              .build())
            .build())
          .build())
        .build())
      .build();
        
    String propertyCount = "Property-Count";
    String propertyDefined = "propertyDefined";
    List<VarBinding> propertyDefinedBindings = getVarBindingsWithProperty( inputDef, propertyDefined);

    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef, null, null);

    // Then..
    TestCase failure = getFailureCase( testDef, propertyCount, "< 2");
    assertThat(
      String.format( "Instances of '%s' in %s", propertyDefined, failure),
      varBindingsMatching( failure, propertyDefinedBindings).size(),
      lessThan(2));

    failure = getFailureCase( testDef, propertyCount, "> 3");
    assertThat(
      String.format( "Instances of '%s' in %s", propertyDefined, failure),
      varBindingsMatching( failure, propertyDefinedBindings).size(),
      greaterThan(3));

    getSuccessCases( testDef, propertyCount, "<= 3")
      .stream()
      .forEach( test -> {
        assertThat(
          String.format( "Instances of '%s' in %s", propertyDefined, test),
          varBindingsMatching( test, propertyDefinedBindings).size(),
          allOf( greaterThanOrEqualTo(2), lessThanOrEqualTo(3)));
          });
    }
  
  @Test
  public void withBetween()
    {
    // Given...
    SystemInputDef inputDef =
      SystemInputDefBuilder.with( "Ice-Cream")
      .functions(
        FunctionInputDefBuilder.with( "Cones")
        .vars(
          VarDefBuilder.with( "Cone")
          .values(
            VarValueDefBuilder.with( "Empty")
            .when( lessThan( "scoop", 1))
            .type( FAILURE)
            .build(),
            
            VarValueDefBuilder.with( "Plain")
            .when( allOf( equalTo( "scoop", 1), notMoreThan( "topping", 1)))
            .build(),
            
            VarValueDefBuilder.with( "Plenty")
            .when( allOf( between( "scoop", 1, 2), notMoreThan( "topping", 2)))
            .build(),
            
            VarValueDefBuilder.with( "Grande")
            .when( allOf( betweenExclusive( "scoop", 0, 4), between( "topping", 1, 3)))
            .build(),
            
            VarValueDefBuilder.with( "Too-Much")
            .when( anyOf( moreThan( "scoop", 3), notLessThan( "topping", 4)))
            .type( FAILURE)
            .build())
          .build(),

          VarSetBuilder.with( "Flavors")
          .members(
            VarDefBuilder.with( "Vanilla")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "scoop").build(),
              VarValueDefBuilder.with( "No").build())
            .build(),
            VarDefBuilder.with( "Chocolate")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "scoop").build(),
              VarValueDefBuilder.with( "No").build())
            .build(),
            VarDefBuilder.with( "Strawberry")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "scoop").build(),
              VarValueDefBuilder.with( "No").build())
            .build(),
            VarDefBuilder.with( "Pistachio")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "scoop").build(),
              VarValueDefBuilder.with( "No").build())
            .build(),
            VarDefBuilder.with( "Lemon")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "scoop").build(),
              VarValueDefBuilder.with( "No").build())
            .build(),
            VarDefBuilder.with( "Coffee")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "scoop").build(),
              VarValueDefBuilder.with( "No").build())
            .build())
          .build(),

          VarSetBuilder.with( "Toppings")
          .when( has( "scoop"))
          .members(
            VarDefBuilder.with( "Sprinkles")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "topping").build(),
              VarValueDefBuilder.with( "No").build())
            .build(),
            VarDefBuilder.with( "Pecans")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "topping").build(),
              VarValueDefBuilder.with( "No").build())
            .build(),
            VarDefBuilder.with( "Oreos")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "topping").build(),
              VarValueDefBuilder.with( "No").build())
            .build(),
            VarDefBuilder.with( "Cherries")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "topping").build(),
              VarValueDefBuilder.with( "No").build())
            .build(),
            VarDefBuilder.with( "MMs")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "topping").build(),
              VarValueDefBuilder.with( "No").build())
            .build(),
            VarDefBuilder.with( "Peppermint")
            .values(
              VarValueDefBuilder.with( "Yes").properties( "topping").build(),
              VarValueDefBuilder.with( "No").build())
            .build())
          .build())
        .build())
      .build();

    // When...
    SystemTestDef testDef = Tcases.getTests( inputDef, null, null);

    // Then..        
    String cone = "Cone";
    String scoop = "scoop";
    List<VarBinding> scoopBindings = getVarBindingsWithProperty( inputDef, scoop);

    String topping = "topping";
    List<VarBinding> toppingBindings = getVarBindingsWithProperty( inputDef, topping);

    TestCase failure = getFailureCase( testDef, cone, "Empty");
    assertThat(
      String.format( "Instances of '%s' in %s", scoop, failure),
      varBindingsMatching( failure, scoopBindings).size(),
      lessThan(1));

    failure = getFailureCase( testDef, cone, "Too-Much");
    assertThat(
      String.format( "Instances of '%s' in %s", scoop, failure),
      varBindingsMatching( failure, scoopBindings).size(),
      greaterThan(3));

    getSuccessCases( testDef, cone, "Plain")
      .stream()
      .forEach( test -> {
        assertThat(
          String.format( "Instances of '%s' in %s", scoop, test),
          varBindingsMatching( test, scoopBindings).size(),
          is( 1));
        assertThat(
          String.format( "Instances of '%s' in %s", topping, test),
          varBindingsMatching( test, toppingBindings).size(),
          lessThanOrEqualTo( 1));
          });

    getSuccessCases( testDef, cone, "Plenty")
      .stream()
      .forEach( test -> {
        assertThat(
          String.format( "Instances of '%s' in %s", scoop, test),
          varBindingsMatching( test, scoopBindings).size(),
          allOf( greaterThanOrEqualTo(1), lessThanOrEqualTo(2)));
        assertThat(
          String.format( "Instances of '%s' in %s", topping, test),
          varBindingsMatching( test, toppingBindings).size(),
          lessThanOrEqualTo( 2));
          });

    getSuccessCases( testDef, cone, "Grande")
      .stream()
      .forEach( test -> {
        assertThat(
          String.format( "Instances of '%s' in %s", scoop, test),
          varBindingsMatching( test, scoopBindings).size(),
          allOf( greaterThan(0), lessThan(4)));
        assertThat(
          String.format( "Instances of '%s' in %s", topping, test),
          varBindingsMatching( test, toppingBindings).size(),
          allOf( greaterThanOrEqualTo(1), lessThanOrEqualTo( 3)));
          });
    }

  /**
   * Returns the success test cases with the given variable binding.
   */
  private List<TestCase> getSuccessCases( SystemTestDef testDef, String varPath, Object value)
    {
    List<TestCase> tests = getTests( testDef, varPath, value);

    tests
      .stream()
      .filter( test -> !test.getVarBinding( varPath).isValueValid())
      .findFirst()
      .ifPresent( failure -> assertThat( String.format( "Unexpected failure with %s=%s", varPath, value), failure, is( nullValue())));

    assertThat( String.format( "Tests with %s=%s", varPath, value), tests.size(), greaterThan( 0));
    return tests;
    }

  /**
   * Returns the failure test case with the given variable binding.
   */
  private TestCase getFailureCase( SystemTestDef testDef, String varPath, Object value)
    {
    List<TestCase> tests = getTests( testDef, varPath, value);
    assertThat( String.format( "Tests with %s=%s", varPath, value), tests.size(), is( 1));

    TestCase test = tests.get(0);
    assertThat( String.format( "Test with %s=%s", varPath, value), test.getType(), is( TestCase.Type.FAILURE));

    return test;
    }

  /**
   * Returns the test cases with the given variable binding.
   */
  private List<TestCase> getTests( SystemTestDef testDef, String varPath, Object value)
    {
    return
      toStream( testDef.getFunctionTestDefs())
      .flatMap( f -> toStream( f.getTestCases()))
      .filter( test -> Optional.ofNullable( test.getVarBinding( varPath)).filter( b -> Objects.equals( b.getValue(), value)).isPresent())
      .collect( toList());
    }

  /**
   * Returns the variable bindings that have the given property.
   */
  private List<VarBinding> getVarBindingsWithProperty( SystemInputDef inputDef, String property)
    {
    return
      toStream( inputDef.getFunctionInputDefs())
      .flatMap( f -> toStream( new VarDefIterator( f)))
      .flatMap( var -> toStream( var.getValues()).map( value -> new VarBindingDef( var, value)))
      .filter( binding -> binding.getValueDef().getProperties().contains( property))
      .map( binding -> VarBinding.create( binding))
      .collect( toList());
    }

  /**
   * Returns the subset of the given bindings that appear in the given test case.
   */
  private List<VarBinding> varBindingsMatching( TestCase testCase, List<VarBinding> bindings)
    {
    return
      toStream( testCase.getVarBindings())
      .filter( binding -> {
        return
          bindings.stream()
          .filter( b -> Objects.equals( b.getVar(), binding.getVar()) && Objects.equals( b.getValue(), binding.getValue()))
          .findFirst()
          .isPresent();
        })
      .collect( toList());
    }
  }

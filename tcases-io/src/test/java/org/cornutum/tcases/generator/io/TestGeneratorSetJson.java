//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.generator.io;

import org.cornutum.tcases.generator.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;

import org.junit.Test;
import org.leadpony.justify.api.JsonValidatingException;
import org.leadpony.justify.api.Problem;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.stream.Collectors.toList;

/**
 * Runs tests for {@link GeneratorSetJsonReader} and {@link GeneratorSetJsonWriter}.
 *
 */
public class TestGeneratorSetJson
  {
  @Test
  public void testGeneratorSet_0()
    {
    testGeneratorSetResource( "generator-set-0.xml");
    }

  @Test
  public void testGeneratorSet_1()
    {
    testGeneratorSetResource( "generator-set-1.xml");
    }

  @Test
  public void testGeneratorSet_2()
    {
    testGeneratorSetResource( "generator-set-2.xml");
    }

  @Test
  public void testGeneratorSet_3()
    {
    testGeneratorSetResource( "generator-set-3.xml");
    }

  @Test
  public void testGeneratorSet_4()
    {
    testGeneratorSetResource( "generator-set-4.xml");
    }

  @Test
  public void testGeneratorSet_5()
    {
    testGeneratorSetResource( "generator-set-once.xml");
    }

  @Test
  public void testGeneratorSet_Combiner_Bindings_Missing()
    {
    assertValidationFailure( "generator-combiner-bindings-missing.json", "The object must have at least 1 property(ies), but actual number is 0");
    }

  @Test
  public void testGeneratorSet_Combiner_Excludes_Missing()
    {
    assertValidationFailure( "generator-combiner-excludes-missing.json", "The array must have at least 1 element(s), but actual number is 0");
    }

  @Test
  public void testGeneratorSet_Combiner_Includes_Missing()
    {
    assertValidationFailure( "generator-combiner-includes-missing.json", "The array must have at least 1 element(s), but actual number is 0");
    }

  @Test
  public void testGeneratorSet_Combiner_Once_Missing()
    {
    assertValidationFailure( "generator-combiner-once-missing.json", "The array must have at least 1 element(s), but actual number is 0");
    }

  @Test
  public void testGeneratorSet_Combiner_Property_Extra()
    {
    assertValidationFailure( "generator-combiner-property-extra.json", "The object must not have a property whose name is \"failure\"");
    }

  @Test
  public void testGeneratorSet_Combiner_Tuples_Invalid()
    {
    assertValidationFailure( "generator-combiner-tuples-invalid.json", "The value must be of integer type, but actual type is number");
    }

  @Test
  public void testGeneratorSet_Generator_Property_Extra()
    {
    assertValidationFailure( "generator-property-extra.json", "The object must not have a property whose name is \"once\"");
    }

  @Test
  public void testGeneratorSet_Generator_Tuples_Invalid()
    {
    assertValidationFailure( "generator-tuples-invalid.json", "The numeric value must be greater than or equal to 0");
    }

  @Test
  public void testGeneratorSet_Exclude_Var_Name_Invalid()
    {
    assertDefinitionError( "generator-exclude-var-invalid.json", "\"c.Some Variable\" is not a valid variable name pattern");
    }

  @Test
  public void testGeneratorSet_Function_Name_Invalid()
    {
    assertDefinitionError( "generator-function-name-invalid.json", "\"My Function\" is not a valid identifier");
    }

  @Test
  public void testGeneratorSet_Include_Var_Name_Invalid()
    {
    assertDefinitionError( "generator-include-var-invalid.json", "\"A.*.Z\" is not a valid variable name pattern");
    }

  @Test
  public void testGeneratorSet_Once_Tuple_Size_Invalid()
    {
    assertDefinitionError( "generator-once-tuple-size-invalid.json", "Once-only tuple=TupleRef[{VarBinding[X5=V5],VarBinding[X6=V6]}] has size=2, expected size=3");
    }

  @Test
  public void testGeneratorSet_Once_Var_Name_Invalid()
    {
    assertDefinitionError( "generator-once-var-invalid.json", "\"Either/Or\" is not a valid identifier");
    }
  
  public void testGeneratorSetResource( String generatorSetResource)
    {
    // Given...
    IGeneratorSet generatorSetBefore = generatorSetResources_.read( generatorSetResource);

    // When...
    ByteArrayOutputStream generatorSetOut = new ByteArrayOutputStream();
    try( GeneratorSetJsonWriter writer = new GeneratorSetJsonWriter( generatorSetOut))
      {
      writer.write( generatorSetBefore);
      }

    IGeneratorSet generatorSetAfter;
    ByteArrayInputStream generatorSetIn = new ByteArrayInputStream( generatorSetOut.toByteArray());
    try( GeneratorSetJsonReader reader = new GeneratorSetJsonReader( generatorSetIn))
      {
      generatorSetAfter = reader.getGeneratorSet();
      }    

    // Then...
    assertThat( "Output from definition=" + generatorSetResource, generatorSetAfter, is( generatorSetBefore));
    }

  public void assertDefinitionError( String generatorSetResource, String expected)
    {
    expectFailure( GeneratorSetException.class)
      .when( () -> generatorSetResources_.readJson( generatorSetResource))
      .then( failure -> {
        while( failure.getCause() != null)
          {
          assertThat( "Cause", failure.getCause().getClass(), equalTo( GeneratorSetException.class));
          failure = (GeneratorSetException) failure.getCause();
          }
        assertThat( "Reason", failure.getMessage(), containsString( expected));
        });
    }

  public void assertValidationFailure( String generatorSetResource, String... expected)
    {
    expectFailure( GeneratorSetException.class)
      .when( () -> generatorSetResources_.readJson( generatorSetResource))
      .then( failure -> {
        Throwable cause = failure.getCause();
        assertThat( "Cause", cause.getClass(), equalTo( JsonValidatingException.class));

        JsonValidatingException jve = (JsonValidatingException)cause;
        List<String> problems = problems( jve).map( p -> p.getMessage()).collect( toList());
        assertThat( "Problems", problems, containsInAnyOrder( Arrays.stream( expected).map( m -> containsString(m)).collect( toList())));
        });
    }

  private Stream<Problem> problems( JsonValidatingException jve)
    {
    return jve.getProblems().stream().flatMap( p -> problems( p));
    }

  private Stream<Problem> problems( Problem problem)
    {
    return
      problem.hasBranches()?
      
      IntStream.range( 0, problem.countBranches())
      .mapToObj( i -> problem.getBranch(i))
      .flatMap( problems -> problems.stream().flatMap( p -> problems( p))) :

      Stream.of( problem);
    }

  private GeneratorSetResources generatorSetResources_ = new GeneratorSetResources( TestGeneratorSetJson.class);
  }

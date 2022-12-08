//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.*;
import org.cornutum.tcases.generator.TupleGenerator;
import static org.cornutum.tcases.resolve.DataValues.*;

import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;

import java.util.Arrays;

/**
 * Runs tests for {@link TestCaseSchemaResolver}
 */
public class TestCaseSchemaResolverTest extends ResolverTest
  {
  @Test
  public void whenVarValuesSchemaIntegerMerged()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarSetBuilder.with( "Parent")
        .members(

          VarDefBuilder.with( "Var")
          .schema(
            SchemaBuilder.type( "integer")
            .format( "int32")
            .exclusiveMinimum( 0)
            .multipleOf( 7)
            .build())

          .values(
            VarValueDefBuilder.with( "X")
            .build(),
                  
            VarValueDefBuilder.with( "Integer")
            .schema(
              SchemaBuilder.type( "integer")
              .format( "int64")
              .minimum( -100)
              .maximum( 100)
              .exclusiveMaximum( 100)
              .build())
            .build(),
                  
            VarValueDefBuilder.with( "Y")
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Parent.Var='X'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "X")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Parent.Var='Integer'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( 56L)
          .source( "Integer")
          .has( "format", "int64")
          .build())
        .build(),

        TestCaseBuilder.with( 2)
        .name( "Parent.Var='Y'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "Y")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaArrayMerged()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")
        .schema(
          SchemaBuilder.type( "array")
          .format( "list")
          .maxItems( 2)
          .items( SchemaBuilder.type( "number").multipleOf( "0.25").build())
          .build())

        .values(
          VarValueDefBuilder.with( "X")
          .build(),
                  
          VarValueDefBuilder.with( "Array")
          .schema(
            SchemaBuilder.type( "array")
            .maxItems( 4)
            .uniqueItems( true)
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='X'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "X")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Var='Array'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( bigDecimals( "-4469906618379619235.75", "-2471231820307629375.25", "-2799448626275174456.00"))
          .source( "Array")
          .has( "format", "list")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaIntegerMinimumNone()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")

        .values(                  
          VarValueDefBuilder.with( "Integer")
          .schema(
            SchemaBuilder.type( "integer")
            .exclusiveMinimum( 100)
            .multipleOf( 13)
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='Integer'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( 2450813644822315045L)
          .source( "Integer")
          .has( "format", "int64")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaStringMaxLengthNone()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarSetBuilder.with( "Parent")
        .members(

          VarDefBuilder.with( "Var")
          .schema(
            SchemaBuilder.type( "string")
            .format( "date-time")
            .build())

          .values(
            VarValueDefBuilder.with( "String")
            .schema(
              SchemaBuilder.type( "string")
              .build())
            .build(),
                  
            VarValueDefBuilder.with( "Y")
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Parent.Var='String'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "2023-08-10T22:02:48.720-05:00")
          .source( "String")
          .has( "format", "date-time")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Parent.Var='Y'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "Y")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaArrayUnmerged()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")

        .values(
          VarValueDefBuilder.with( "Array")
          .schema(
            SchemaBuilder.type( "array")
            .format( "flags")
            .minItems( 3)
            .items(
              SchemaBuilder.type( "boolean")
              .constant( true)
              .build())
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='Array'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( Arrays.asList( true, true, true, true, true, true, true, true, true, true, true, true, true))
          .source( "Array")
          .has( "format", "flags")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaNumberMaximum()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarSetBuilder.with( "Parent")
        .members(

          VarDefBuilder.with( "Var")
          .schema(
            SchemaBuilder.type( "boolean")
            .build())

          .values(
            VarValueDefBuilder.with( "Number")
            .schema(
              SchemaBuilder.type( "number")
              .format( "double")
              .minimum( "12.3")
              .exclusiveMinimum( "12.3")
              .maximum( "123.4")
              .build())
            .build(),
                  
            VarValueDefBuilder.with( "Y")
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Parent.Var='Number'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( bigDecimalOf( "41.6"))
          .source( "Number")
          .has( "format", "double")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Parent.Var='Y'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "Y")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaNumberMerged()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")
        .schema(
          SchemaBuilder.type( "number")
          .format( "float")
          .minimum( "-3.14")
          .maximum( "3.14")
          .exclusiveMaximum( "3.14156")
          .build())

        .values(
          VarValueDefBuilder.with( "Number")
          .schema(
            SchemaBuilder.type( "number")
            .exclusiveMaximum( "1.415")
            .multipleOf( "0.005")
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='Number'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( bigDecimalOf( "-0.445"))
          .source( "Number")
          .has( "format", "float")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaBooleanUnmerged()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")
        .schema(
          SchemaBuilder.type( "string")
          .format( "email")
          .minLength( 16)
          .maxLength( 32)
          .build())

        .values(
          VarValueDefBuilder.with( "Boolean")
          .schema(
            SchemaBuilder.type( "boolean")
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='Boolean'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( true)
          .source( "Boolean")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaStringMerged()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarSetBuilder.with( "Parent")
        .members(

          VarDefBuilder.with( "Var")
          .schema(
            SchemaBuilder.type( "string")
            .format( "uuid")
            .build())

          .values(
            VarValueDefBuilder.with( "String")
            .schema(
              SchemaBuilder.type( "string")
              .format( "date")
              .build())
            .build(),
                  
            VarValueDefBuilder.with( "Y")
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Parent.Var='String'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "2023-02-04")
          .source( "String")
          .has( "format", "date")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Parent.Var='Y'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "Y")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaStringUuid()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")

        .values(
          VarValueDefBuilder.with( "String")
          .schema(
            SchemaBuilder.type( "string")
            .format( "uuid")
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='String'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "3c15d95d-e89c-4d0f-9aa2-85c2055ea3ed")
          .source( "String")
          .has( "format", "uuid")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaStringDateTime()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")

        .values(
          VarValueDefBuilder.with( "String")
          .schema(
            SchemaBuilder.type( "string")
            .format( "date-time")
            .build())
          .build(),

          VarValueDefBuilder.with( "Y")
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='String'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "2023-08-10T22:02:48.720-05:00")
          .source( "String")
          .has( "format", "date-time")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Var='Y'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "Y")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaStringMaxLength()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarSetBuilder.with( "Parent")
        .members(

          VarDefBuilder.with( "Var")
          .schema(
            SchemaBuilder.type( "number")
            .exclusiveMinimum( 0)
            .multipleOf( 10)
            .build())

          .values(
            VarValueDefBuilder.with( "String")
            .schema(
              SchemaBuilder.type( "string")
              .format( "email")
              .minLength( 16)
              .maxLength( 32)
              .build())
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Parent.Var='String'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "w.?.W.Q@kDta.com")
          .source( "String")
          .has( "format", "email")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaStringPattern()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")

        .values(
          VarValueDefBuilder.with( "X")
          .build(),
                  
          VarValueDefBuilder.with( "String")
          .schema(
            SchemaBuilder.type( "string")
            .pattern( "Hello, (there|you|world)")
            .minLength( 1)
            .maxLength( 16)
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='X'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "X")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Var='String'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "?5^Hello, you[")
          .source( "String")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaConstNull()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")

        .values(
          VarValueDefBuilder.with( "Integer")
          .schema(
            SchemaBuilder.type( "integer")
            .constant( noValue())
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='Integer'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( null)
          .source( "Integer")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaConstArray()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarSetBuilder.with( "Parent")
        .members(

          VarDefBuilder.with( "Var")
          .schema(
            SchemaBuilder.type( "boolean")
            .build())

          .values(
            VarValueDefBuilder.with( "Array")
            .schema(
              SchemaBuilder.type( "array")
              .constant( arrayOf( 3, 1, 4, 1, 5, 6))
              .build())
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Parent.Var='Array'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( Arrays.asList( 3, 1, 4, 1, 5, 6))
          .source( "Array")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarSchemaDefinesInteger()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")
        .schema(
          SchemaBuilder.type( "integer")
          .format( "int32")
          .minimum( -123)
          .maximum( 456)
          .exclusiveMaximum( 456)
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='minimum'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( -123)
          .source( "minimum")
          .has( "format", "int32")
          .build())
        .build(),
        
        TestCaseBuilder.with( 1)
        .name( "Var='maximum'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( 455)
          .source( "maximum")
          .has( "format", "int32")
          .build())
        .build(),

        TestCaseBuilder.with( 2)
        .name( "Var='belowMinimum'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( -286487905)
          .valid( false)
          .source( "belowMinimum")
          .has( "format", "int32")
          .build())
        .build(),

        TestCaseBuilder.with( 3)
        .name( "Var='aboveMaximum'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( 132042812)
          .valid( false)
          .source( "aboveMaximum")
          .has( "format", "int32")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarSchemaDefinesArray()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")
        .schema(
          SchemaBuilder.type( "array")
          .minItems( 3)
          .maxItems( 8)
          .uniqueItems( false)
          .items( SchemaBuilder.type( "integer").minimum( -10).maximum( 10).build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='minimumSize'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( Arrays.asList( 0, 7, 0))
          .source( "minimumSize")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Var='maximumSize'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( Arrays.asList( -10, 9, 3, -6, 3, 3, 7, 9))
          .source( "maximumSize")
          .build())
        .build(),

        TestCaseBuilder.with( 2)
        .name( "Var='tooSmall'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( Arrays.asList( -2, -2))
          .valid( false)
          .source( "tooSmall")
          .build())
        .build(),

        TestCaseBuilder.with( 3)
        .name( "Var='tooLarge'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( Arrays.asList( 4, -7, -9, 0, -8, -5, -2, -1, -5, -1))
          .valid( false)
          .source( "tooLarge")
          .build())
        .build(),

        TestCaseBuilder.with( 4)
        .name( "Var='wrongItems'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( bigDecimals( "-388.61", "-596.14", "998.27", "-964.84", "106.39", "-388.61"))
          .valid( false)
          .source( "wrongItems")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarSchemaDefinesString()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")
        .schema(
          SchemaBuilder.type( "string")
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='empty'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "")
          .source( "empty")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Var='anyLength'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "S0)$yhhlf~$]%`4'D{wM*8*b<L3%IRa|8C(`KgUnbd:8?1zt\\uM}4-i+U~pW?5^HEi.\\<S)^dq%k\"dTq+>gy-HZt6rZ%Wz")
          .source( "anyLength")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesSchemaNone()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarSetBuilder.with( "Parent")
        .members(

          VarDefBuilder.with( "Var")
          .schema(
            SchemaBuilder.type( "string")
            .format( "date")
            .build())

          .values(
            VarValueDefBuilder.with( "X")
            .build(),
                  
            VarValueDefBuilder.with( "Y")
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Parent.Var='X'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "X")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Parent.Var='Y'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "Y")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarSchemaDefinesNumber()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")
        .schema(
          SchemaBuilder.type( "number")
          .format( "float")
          .minimum( "-123.45")
          .maximum( "123.45")
          .exclusiveMaximum(  "123.45")
          .multipleOf( "12.3")
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Var='minimum'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "-123.0")
          .source( "minimum")
          .has( "format", "float")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Var='maximum'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "110.7")
          .source( "maximum")
          .has( "format", "float")
          .build())
        .build(),

        TestCaseBuilder.with( 2)
        .name( "Var='belowMinimum'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "-4527733255718008336.8")
          .valid( false)
          .source( "belowMinimum")
          .has( "format", "float")
          .build())
        .build(),

        TestCaseBuilder.with( 3)
        .name( "Var='aboveMaximum'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "3192900771962155300.2")
          .valid( false)
          .source( "aboveMaximum")
          .has( "format", "float")
          .build())
        .build(),

        TestCaseBuilder.with( 4)
        .name( "Var='notMultiple'")
        .bind(
          VarBindingBuilder.with( "Var")
          .value( "110.6")
          .valid( false)
          .source( "notMultiple")
          .has( "format", "float")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarSchemaDefinesPattern()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarSetBuilder.with( "Parent")
        .members(

          VarDefBuilder.with( "Var")
          .schema(
            SchemaBuilder.type( "string")
            .minLength( 8)
            .maxLength( 16)
            .pattern( "^[\\w]+$")
            .build())
          .build())
        .build())
      .build();
    
    // When...
    FunctionTestDef testDef = resolveTests( inputDef);
    
    // Then...
    FunctionTestDef expected =
      FunctionTestDefBuilder.with( "Model")
      .testCases(
        TestCaseBuilder.with( 0)
        .name( "Parent.Var='minimumLength'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "QJ6uYHb6")
          .source( "minimumLength")
          .build())
        .build(),

        TestCaseBuilder.with( 1)
        .name( "Parent.Var='maximumLength'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "vHLkE8EX9As94fft")
          .source( "maximumLength")
          .build())
        .build(),

        TestCaseBuilder.with( 2)
        .name( "Parent.Var='tooShort'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "zC")
          .valid( false)
          .source( "tooShort")
          .build())
        .build(),

        TestCaseBuilder.with( 3)
        .name( "Parent.Var='tooLong'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "VXOLCa1pbdSFv9r_KvBl6l1eqUdXXOw1TGAkcnWPonC")
          .valid( false)
          .source( "tooLong")
          .build())
        .build(),

        TestCaseBuilder.with( 4)
        .name( "Parent.Var='wrongPattern'")
        .bind(
          VarBindingBuilder.with( "Parent.Var")
          .value( "'\",+:+,[ '|\"%")
          .valid( false)
          .source( "wrongPattern")
          .build())
        .build())
      .build();

    assertThat( "Resolved tests", testDef, matches( new FunctionTestDefMatcher( expected)));
    }
  
  @Test
  public void whenVarValuesNone()
    {
    // Given...
    FunctionInputDef inputDef =
      FunctionInputDefBuilder.with( "Model")
      .vars(
        VarDefBuilder.with( "Var")
        .build())
      .build();

    assertResolverException(
      // When...
      () -> resolveTests( inputDef),

      // Then...
      "Error processing Model, Var",
      "No schema or values defined for this variable");
    }

  private FunctionTestDef resolveTests( FunctionInputDef inputDef)
    {
    return
      Tcases.getTests(
        inputDef,
        new TupleGenerator(),
        new TestCaseSchemaResolver( getResolverContext()),
        null,
        null);
    }
  }

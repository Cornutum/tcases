//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import static org.cornutum.tcases.openapi.SchemaUtils.*;

import io.swagger.v3.oas.models.media.Schema;
import org.junit.Test;
import static org.cornutum.hamcrest.Composites.*;
import static org.cornutum.hamcrest.ExpectedFailure.expectFailure;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

/**
 * Runs tests for {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()}.
 */
public class CombineObjectSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( 10)
      .minProperties( 1)
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 11)
      .minProperties( 0)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( 10)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( 10)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( null)
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( 10)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( null)
      .minProperties( null)
      .required( "Delta")
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie", "Delta")
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( 2)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( 100)
      .minProperties( 10)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( false)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( 100)
      .minProperties( 10)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Missing </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Different </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 9)
      .minProperties( 0)
      .required( "Alpha")
      .property( "Alpha", SchemaBuilder.ofType( "string").patterns( ".*").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( 9)
      .minProperties( 0)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").patterns( ".*").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Extra </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 10)
      .minProperties( 1)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( true)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 11)
      .minProperties( 0)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Delta", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 10)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .property( "Delta", SchemaBuilder.ofType( "string").build())
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( null)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( false)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( null)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( true)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( null)
      .minProperties( null)
      .required( "Delta")
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie", "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( null)
      .minProperties( 1)
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 10)
      .minProperties( 9)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( 10)
      .minProperties( 9)
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Different </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_9()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( true)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 9)
      .minProperties( 8)
      .required( "Alpha", "Bravo")
      .property( "Alpha", SchemaBuilder.ofType( "string").minLength( 1).build())
      .property( "Bravo", SchemaBuilder.ofType( "number").maximum( 1.23).build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").minimum( 0).build())
      .additionalProperties( false)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 9)
      .minProperties( 8)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").minLength( 1).build())
      .property( "Bravo", SchemaBuilder.ofType( "number").maximum( 1.23).build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").minimum( 0).build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Missing </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_10()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( 10)
      .minProperties( 1)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( true)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( 11)
      .minProperties( 0)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( 10)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Extra </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Different </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_11()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( 2)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").maxLength(1).build())
      .property( "Delta", SchemaBuilder.ofType( "array").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( true)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( null)
      .minProperties( 2)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").maxLength(1).build())
      .property( "Delta", SchemaBuilder.ofType( "array").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_12()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( null)
      .required( "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie", "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_13()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( 10)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( 11)
      .additionalProperties( true)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( 11)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_14()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( 9)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Delta")
      .additionalProperties( SchemaBuilder.ofType( "object").required( "Foo").build())
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( 9)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie", "Delta")
      .additionalProperties( SchemaBuilder.ofType( "object").required( "Foo").build())
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_15()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( 10)
      .minProperties( 1)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( 11)
      .minProperties( 0)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( false)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .format( "otherFormat")
      .maxProperties( 10)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Missing </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> <FONT color="red"> Inconsistent  </FONT> </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_16()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( null)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( null)
      .property( "Bravo", SchemaBuilder.ofType( "array").build())
      .additionalProperties( true)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    expectFailure( OpenApiException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat( "Failure", failure.getMessage(), is( "Error processing properties, Bravo"));
        
        Throwable cause = failure.getCause();
        assertThat( "Cause", cause, is( instanceOf( IllegalStateException.class)));
        assertThat( "Cause", cause.getMessage(), is( "Can't combine schema of type=array with base schema of type=number"));
        });        
    }

  /**
   * Tests {@link SchemaUtils#combineObjectSchemas combineObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 17. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Missing </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Different </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> <FONT color="red"> Inconsistent  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_17()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .format( "baseFormat")
      .maxProperties( null)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .property( "Charlie", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .format( null)
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").minLength(0).build())
      .additionalProperties( SchemaBuilder.ofType( "integer").build())
      .build();

    OpenApiContext context = new OpenApiContext();
    
    expectFailure( OpenApiException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat( "Failure", failure.getMessage(), is( "Error processing additionalProperties"));
        
        Throwable cause = failure.getCause();
        assertThat( "Cause", cause, is( instanceOf( IllegalStateException.class)));
        assertThat( "Cause", cause.getMessage(), is( "Can't combine schema of type=integer with base schema of type=object"));
        });
    }
  }

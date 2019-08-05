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
 * Runs tests for {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()}.
 */
public class CombineNotObjectSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( null)
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( 2)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Different </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 1)
      .property( "Alpha", SchemaBuilder.ofType( "number").multipleOf( 2).build())
      .additionalProperties( true)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "number").multipleOf( 3).build())
      .additionalProperties( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 1)
      .property( "Alpha", SchemaBuilder.ofType( "number").notMultipleOfs( 2, 3).build())
      .additionalProperties( true)
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Extra </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 100)
      .minProperties( 10)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( SchemaBuilder.ofType( "object").required( "X").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 1)
      .required( "Bravo", "Charlie", "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Charlie", SchemaBuilder.ofType( "number").build())
      .additionalProperties( SchemaBuilder.ofType( "object").required( "Y").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 100)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie", "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Charlie", SchemaBuilder.ofType( "number").build())
      .additionalProperties( SchemaBuilder.ofType( "object").required( "X", "Y").build())
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additionanl.maxProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Missing </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Different </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 2)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Charlie", SchemaBuilder.ofType( "number").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( 100)
      .minProperties( 10)
      .required( "Easy")
      .property( "Alpha", SchemaBuilder.ofType( "string").format( "something").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 100)
      .minProperties( 2)
      .required( "Alpha", "Bravo", "Charlie", "Easy")
      .property( "Alpha", SchemaBuilder.ofType( "string").notFormats( "something").build())
      .property( "Charlie", SchemaBuilder.ofType( "number").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( 9)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 10)
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( 100)
      .minProperties( null)
      .additionalProperties( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 100)
      .minProperties( 10)
      .additionalProperties( true)
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 2)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 1)
      .required( "Charlie", "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie", "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Extra </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Different </TD> </TR>
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
      .maxProperties( 10)
      .minProperties( 5)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "object").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( 100)
      .minProperties( 50)
      .required( "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "object").format( "another").build())
      .property( "Delta", SchemaBuilder.ofType( "integer").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 100)
      .minProperties( 5)
      .required( "Alpha", "Bravo", "Charlie", "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "object").notFormats( "another").build())
      .property( "Delta", SchemaBuilder.ofType( "integer").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Missing </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
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
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
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
      .maxProperties( 10)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( 5)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_11()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 1)
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 1)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> None </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_12()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 5)
      .required( "Alpha", "Bravo", "Charlie")
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 1)
      .required( "Alpha", "Charlie")
      .additionalProperties( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( true)
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Different </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_13()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( 11)
      .minProperties( 2)
      .required( "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "string").format( "base64").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 11)
      .minProperties( 1)
      .required( "Alpha", "Bravo", "Charlie", "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "string").notFormats( "base64").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Extra </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> <FONT color="red"> Inconsistent  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_14()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .additionalProperties( SchemaBuilder.ofType( "array").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing additionalProperties",
      "Can't combine schema of type=array with base schema of type=object");
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Extra </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_15()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .additionalProperties( false)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> combineNotSchemas( context, base, additional))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {not: {additionalProperties: false}} with base schema requiring {not: {additionalProperties: true}}")));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Extra </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_16()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .additionalProperties( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> combineNotSchemas( context, base, additional))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {not: {additionalProperties: true}} with base schema requiring {not: {additionalProperties: false}}")));
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 17. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> <FONT color="red"> 0  </FONT> </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Extra </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_17()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 0)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing ",
      "\"not: {minProperties: 0}\" assertion can't be satisfied by any instance");
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 18. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Extra </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> <FONT color="red"> Inconsistent  </FONT> </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_18() {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "integer").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .additionalProperties( SchemaBuilder.ofType( "array").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing properties, Alpha",
      "Can't combine schema of type=integer with base schema of type=string");
    }

  /**
   * Tests {@link SchemaUtils#combineNotObjectSchemas combineNotObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 19. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> <FONT color="red"> 0  </FONT> </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.required </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.properties.members </TD> <TD> Extra </TD> </TR>
   * <TR><TD> additional.properties.schema </TD> <TD> Same </TD> </TR>
   * <TR><TD> additional.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_19()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 0)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 1)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .property( "Bravo", SchemaBuilder.ofType( "number").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing ",
      "\"not: {minProperties: 0}\" assertion can't be satisfied by any instance");
    }
  }

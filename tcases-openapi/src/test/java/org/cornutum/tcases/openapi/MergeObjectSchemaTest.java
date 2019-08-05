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
 * Runs tests for {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()}.
 */
public class MergeObjectSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.required </TD> <TD> null </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .property( "Beta", SchemaBuilder.ofType( "string").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .notProperty( "Beta", SchemaBuilder.ofType( "string").build())
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> >= baseMaxProperties </TD> </TR>
   * <TR><TD> not.required </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 3)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( SchemaBuilder.ofType( "number").build())
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 10)
      .required( "Charlie")
      .additionalProperties( SchemaBuilder.ofType( "number").minimum( 1.23).build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 9)
      .minProperties( 3)
      .required( "Alpha", "Bravo")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( SchemaBuilder.ofType( "number").build())
      .notAdditionalProperties( SchemaBuilder.ofType( "number").minimum( 1.23).build())
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> > baseMinProperties </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.required </TD> <TD> null </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 4)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( false)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 4)
      .minProperties( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 5)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( true)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 1)
      .minProperties( 10)
      .required( "Delta")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 9)
      .minProperties( 2)
      .required( "Alpha", "Bravo", "Charlie")
      .notProperty( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> >= baseMaxProperties </TD> </TR>
   * <TR><TD> not.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 1)
      .minProperties( 11)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Beta", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 2)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .notProperty( "Beta", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> <= baseMinProperties </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.required </TD> <TD> null </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 5)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 4)
      .minProperties( 10)
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 9)
      .minProperties( 5)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> > baseMinProperties </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.required </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 2)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( true)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 3)
      .minProperties( null)
      .required( "Alpha", "Bravo")
      .additionalProperties( false)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 4)
      .required( "Charlie")
      .additionalProperties( true)
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> <= baseMinProperties </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> < baseMaxProperties </TD> </TR>
   * <TR><TD> not.required </TD> <TD> null </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> true </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 5)
      .required( "Alpha", "Bravo", "Charlie")
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 2)
      .minProperties( 4)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 3)
      .minProperties( 5)
      .required( "Alpha", "Bravo", "Charlie")
      .notProperty( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> < baseMaxProperties </TD> </TR>
   * <TR><TD> not.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 9)
      .required( "Easy")
      .property( "Beta", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 8)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .notProperty( "Beta", SchemaBuilder.ofType( "string").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> false </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_9()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( false)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> > baseMinProperties </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> >= baseMaxProperties </TD> </TR>
   * <TR><TD> not.required </TD> <TD> null </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_10()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 5)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 6)
      .minProperties( 20)
      .additionalProperties( SchemaBuilder.ofType( "array").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 7)
      .notAdditionalProperties( SchemaBuilder.ofType( "array").build())
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.required </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> Schema </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_11()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .additionalProperties( true)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 20)
      .required( "Alpha")
      .property( "Beta", SchemaBuilder.ofType( "string").build())
      .additionalProperties( SchemaBuilder.ofType( "integer").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 19)
      .minProperties( 11)
      .required( "Bravo", "Charlie")
      .notProperty( "Beta", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .notAdditionalProperties( SchemaBuilder.ofType( "integer").build())
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> >= baseMaxProperties </TD> </TR>
   * <TR><TD> not.required </TD> <TD> null </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_12()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( null)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 100)
      .property( "Beta", SchemaBuilder.ofType( "string").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 11)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .notProperty( "Beta", SchemaBuilder.ofType( "string").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Object schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> true </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> <= baseMinProperties </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_13()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 10)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( true)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 12)
      .required( "Fox")
      .additionalProperties( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when( () -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{additionalProperties: true} is not consistent with {not: {additionalProperties: true}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> false </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> <= baseMinProperties </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_14()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 10)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( false)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 12)
      .required( "Fox")
      .additionalProperties( false)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when( () -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{additionalProperties: false} is not consistent with {not: {additionalProperties: false}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeObjectSchemas mergeObjectSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxProperties </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minProperties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.required </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.properties </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.additionalProperties </TD> <TD> Schema </TD> </TR>
   * <TR><TD> not.maxProperties </TD> <TD> <= baseMinProperties </TD> </TR>
   * <TR><TD> not.minProperties </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.required </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.properties </TD> <TD> null </TD> </TR>
   * <TR><TD> not.additionalProperties </TD> <TD> <FONT color="red"> trueUnmergeable  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_15()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "object")
      .maxProperties( null)
      .minProperties( 10)
      .required( "Alpha", "Bravo", "Charlie")
      .property( "Alpha", SchemaBuilder.ofType( "string").build())
      .additionalProperties( SchemaBuilder.ofType( "object").build())
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "object")
      .maxProperties( 10)
      .minProperties( 12)
      .required( "Fox")
      .additionalProperties( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when( () -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{additionalProperties: <schema>} is not consistent with {not: {additionalProperties: true}}")));
    }
  }

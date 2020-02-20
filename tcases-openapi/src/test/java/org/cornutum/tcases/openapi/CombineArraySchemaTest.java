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
 * Runs tests for {@link SchemaUtils#combineArraySchemas combineArraySchemas()}
 */
public class CombineArraySchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> null </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( false)
      .items( SchemaBuilder.empty().build())
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .maxItems( 1000)
      .minItems( 100)
      .uniqueItems( false)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .maxItems( 100)
      .minItems( 100)
      .uniqueItems( false)
      .items( SchemaBuilder.empty().build())
      .build();
    
    assertThat( "Array schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-empty </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .nullable( true)
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "string").build())
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( true)
      .maxItems( 200)
      .minItems( 1)
      .items( SchemaBuilder.empty().build())
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( true)
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "string").build())
      .build();
    
    assertThat( "Array schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .nullable( true)
      .uniqueItems( false)
      .items( SchemaBuilder.empty().build())
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "integer").build())
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( true)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "integer").build())
      .build();
    
    assertThat( "Array schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-empty </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> null </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .nullable( false)
      .maxItems( 1000)
      .minItems( 100)
      .items( SchemaBuilder.ofType( "object").build())
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .uniqueItems( false)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .nullable( false)
      .uniqueItems( false)
      .maxItems( 1000)
      .minItems( 100)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Array schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-empty </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .nullable( false)
      .minItems( 10)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .nullable( false)
      .maxItems( 11)
      .minItems( 1)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .nullable( false)
      .maxItems( 11)
      .minItems( 10)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();
    
    assertThat( "Array schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .maxItems( 123)
      .minItems( 45)
      .uniqueItems( true)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( true)
      .maxItems( 789)
      .minItems( 46)
      .items( SchemaBuilder.empty().build())
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( true)
      .maxItems( 123)
      .minItems( 46)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();
    
    assertThat( "Array schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .nullable( false)
      .maxItems( 123)
      .items( SchemaBuilder.empty().build())
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( false)
      .maxItems( 100)
      .minItems( 20)
      .items( SchemaBuilder.empty().build())
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( false)
      .maxItems( 100)
      .minItems( 20)
      .items( SchemaBuilder.empty().build())
      .build();
    
    assertThat( "Array schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> null </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .nullable( true)
      .uniqueItems( true)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .nullable( true)
      .uniqueItems( true)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .nullable( true)
      .uniqueItems( true)
      .build();
    
    assertThat( "Array schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .maxItems( 123)
      .minItems( 45)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .nullable( false)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "string").build())
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .nullable( false)
      .maxItems( 123)
      .minItems( 45)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "string").build())
      .build();
    
    assertThat( "Array schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> null </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_9()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .uniqueItems( false)
      .minItems( 42)
      .uniqueItems( false)
      .items( SchemaBuilder.empty().build())
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .maxItems( 84)
      .minItems( 1)
      .uniqueItems( false)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .maxItems( 84)
      .minItems( 42)
      .uniqueItems( false)
      .items( SchemaBuilder.empty().build())
      .build();
    
    assertThat( "Array schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineArraySchemas combineArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-empty </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> <FONT color="red"> Different-Type  </FONT> </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_11()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .nullable( true)
      .maxItems( 123)
      .minItems( 12)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "integer").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( true)
      .maxItems( 124)
      .minItems( 13)
      .items( SchemaBuilder.ofType( "boolean").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    expectFailure( OpenApiException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat( "Failure", failure.getMessage(), is( "Error processing items"));

        Throwable cause = failure.getCause();
        assertThat( "Cause", cause, is( instanceOf( IllegalStateException.class)));
        assertThat( "Cause", cause.getMessage(), is( "Can't combine schema requiring {type: boolean} with schema requiring {type: integer}"));
        });
    }

  @Test
  public void whenInconsistentUniqueItems()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .minItems( 42)
      .uniqueItems( false)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .maxItems( 84)
      .minItems( 1)
      .uniqueItems( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {uniqueItems: true} with schema requiring {uniqueItems: false}"));
        });
    }
  
  @Test
  public void whenInconsistentNullable()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .nullable( false)
      .maxItems( 123)
      .minItems( 45)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .nullable( true)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "string").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {nullable: true} with schema requiring {nullable: false}"));
        });
    }
  }

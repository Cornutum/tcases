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
 * Runs tests for {@link SchemaUtils#combineNotArraySchemas combineNotArraySchemas()}
 */
public class CombineNotArraySchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#combineNotArraySchemas combineNotArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( false)
      .readOnly( false)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( null)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( false)
      .maxItems( 50)
      .minItems( null)
      .uniqueItems( null)
      .items( SchemaBuilder.ofType( "string").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( false)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( null)
      .items( SchemaBuilder.ofType( "string").build())
      .build();
    
    assertThat( "Array not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotArraySchemas combineNotArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( true)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( null)
      .readOnly( false)
      .writeOnly( true)
      .maxItems( null)
      .minItems( 10)
      .uniqueItems( true)
      .items( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat", "otherFormat")
      .nullable( null)
      .readOnly( false)
      .writeOnly( true)
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();
    
    assertThat( "Array not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotArraySchemas combineNotArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 1 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .nullable( false)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( null)
      .minItems( 10)
      .uniqueItems( true)
      .items( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "anotherFormat", "otherFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( null)
      .maxItems( 50)
      .minItems( 5)
      .uniqueItems( null)
      .items( SchemaBuilder.empty().build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .notFormats( "baseFormat", "anotherFormat", "otherFormat")
      .nullable( false)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( 50)
      .minItems( 5)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();
    
    assertThat( "Array not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotArraySchemas combineNotArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 1 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .nullable( null)
      .readOnly( false)
      .writeOnly( null)
      .maxItems( null)
      .minItems( 10)
      .uniqueItems( null)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( false)
      .readOnly( false)
      .writeOnly( true)
      .maxItems( null)
      .minItems( 20)
      .uniqueItems( true)
      .items( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .notFormats( "baseFormat")
      .nullable( false)
      .readOnly( false)
      .writeOnly( true)
      .maxItems( null)
      .minItems( 10)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();
    
    assertThat( "Array not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotArraySchemas combineNotArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( null)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( null)
      .items( SchemaBuilder.ofType( "integer").minimum( 100).build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( false)
      .readOnly( null)
      .writeOnly( false)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( null)
      .items( SchemaBuilder.ofType( "integer").minimum( 200).build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( false)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( null)
      .items( SchemaBuilder.ofType( "integer").minimum( 100).build())
      .build();
    
    assertThat( "Array not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotArraySchemas combineNotArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( true)
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( true)
      .items( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( null)
      .maxItems( 50)
      .minItems( null)
      .uniqueItems( true)
      .items( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .notFormats()
      .nullable( null)
      .readOnly( true)
      .writeOnly( true)
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( true)
      .items( null)
      .build();
    
    assertThat( "Array not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotArraySchemas combineNotArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( false)
      .readOnly( null)
      .writeOnly( null)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( true)
      .items( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( null)
      .readOnly( true)
      .writeOnly( null)
      .maxItems( null)
      .minItems( 10)
      .uniqueItems( null)
      .items( SchemaBuilder.ofType( "object").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .notFormats( "otherFormat")
      .nullable( false)
      .readOnly( true)
      .writeOnly( null)
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Array not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotArraySchemas combineNotArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 1 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( "baseFormat")
      .nullable( null)
      .readOnly( false)
      .writeOnly( false)
      .maxItems( null)
      .minItems( 10)
      .uniqueItems( null)
      .items( SchemaBuilder.ofType( "string").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .format( "otherFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( 5)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .notFormats( "baseFormat", "otherFormat")
      .nullable( false)
      .readOnly( false)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( 5)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "string").build())
      .build();
    
    assertThat( "Array not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotArraySchemas combineNotArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( true)
      .maxItems( null)
      .minItems( 10)
      .uniqueItems( null)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "otherFormat", "anotherFormat")
      .nullable( false)
      .readOnly( false)
      .writeOnly( true)
      .maxItems( null)
      .minItems( 100)
      .uniqueItems( null)
      .items( SchemaBuilder.empty().build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat", "otherFormat", "anotherFormat")
      .nullable( false)
      .readOnly( false)
      .writeOnly( true)
      .maxItems( null)
      .minItems( 10)
      .uniqueItems( null)
      .items( SchemaBuilder.empty().build())
      .build();
    
    assertThat( "Array not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_9()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( true)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( false)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
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
          is( "Can't combine schema requiring {not: {writeOnly: false}} with base schema requiring {not: {writeOnly: true}}")));
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> <FONT color="red"> 0  </FONT> </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_10()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( 0)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( null)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing ",
      "\"not: {minItems: 0}\" assertion can't be satisfied by any instance");
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> <FONT color="red"> 0  </FONT> </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_11()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( null)
      .maxItems( 200)
      .minItems( 0)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing ",
      "\"not: {minItems: 0}\" assertion can't be satisfied by any instance");
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> <FONT color="red"> false  </FONT> </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_12()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( null)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing ",
      "\"not: {uniqueItems: false}\" assertion can't be satisfied by any instance");
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> <FONT color="red"> true  </FONT> </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_13()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( 0)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( true)
      .readOnly( null)
      .writeOnly( null)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing ",
      "\"not: {nullable: true}\" assertion can't be satisfied by any instance");
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_14()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( false)
      .writeOnly( null)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
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
          is( "Can't combine schema requiring {not: {readOnly: false}} with base schema requiring {not: {readOnly: true}}")));
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> <FONT color="red"> true  </FONT> </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_15()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( true)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( null)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing ",
      "\"not: {nullable: true}\" assertion can't be satisfied by any instance");
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_16()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( true)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( false)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
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
          is( "Can't combine schema requiring {not: {writeOnly: false}} with base schema requiring {not: {writeOnly: true}}")));
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 17. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> <FONT color="red"> false  </FONT> </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_17()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( false)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( null)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing ",
      "\"not: {uniqueItems: false}\" assertion can't be satisfied by any instance");
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 18. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> <FONT color="red"> Different-Type  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_18()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "string").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( null)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing items",
      "Can't combine schema of type=number with base schema of type=string");
    }

  /**
   * Tests {@link ArraySchemas#Combine Combine()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 19. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.notFormats </TD> <TD> 0 </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.maxItems </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Empty </TD> </TR>
   * <TR><TD> additional.notFormats </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxItems </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.items </TD> <TD> Non-empty </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_19()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .format( null)
      .nullable( null)
      .readOnly( true)
      .writeOnly( false)
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.empty().build())
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "array")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( false)
      .writeOnly( null)
      .maxItems( 200)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
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
          is( "Can't combine schema requiring {not: {readOnly: false}} with base schema requiring {not: {readOnly: true}}")));
    }
  }

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
 * Runs tests for {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()}.
 */
public class MergeStringSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Contains base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> > baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( true)
      .readOnly( false)
      .writeOnly( null)
      .enums( "Alpha", "Bravo")
      .maxLength( null)
      .minLength( 0)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( null)
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 1)
      .minLength( 10)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .notFormats()
      .nullable( true)
      .readOnly( false)
      .writeOnly( null)
      .notEnums( "Alpha", "Bravo", "Charlie")
      .maxLength( 9)
      .minLength( 2)
      .notPatterns( "A.*", "B.*")
      .build();
    
    assertThat( "String schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( null)
      .nullable( true)
      .readOnly( null)
      .writeOnly( false)
      .enums( "Alpha", "Bravo")
      .maxLength( null)
      .minLength( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .nullable( false)
      .readOnly( null)
      .writeOnly( null)
      .maxLength( null)
      .minLength( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( null)
      .nullable( true)
      .readOnly( null)
      .writeOnly( false)
      .enums( "Alpha", "Bravo")
      .maxLength( null)
      .minLength( null)
      .build();
    
    assertThat( "String schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> >= base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( null)
      .readOnly( true)
      .writeOnly( null)
      .enums( "Alpha", "Bravo")
      .maxLength( 100)
      .minLength( null)
      .patterns( "[A-Z]*", "Z*")
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .nullable( null)
      .readOnly( false)
      .writeOnly( true)
      .enums( "Charlie")
      .maxLength( 1)
      .minLength( 101)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( null)
      .readOnly( true)
      .writeOnly( false)
      .enums( "Alpha", "Bravo")
      .maxLength( 100)
      .minLength( 2)
      .patterns( "[A-Z]*", "Z*")
      .notPatterns( "A.*", "B.*")
      .build();
    
    assertThat( "String schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> <= baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .nullable( null)
      .readOnly( false)
      .writeOnly( false)
      .maxLength( 100)
      .minLength( 10)
      .patterns( "[A-Z]*")
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .nullable( true)
      .readOnly( true)
      .writeOnly( true)
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 10)
      .minLength( 20)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .nullable( false)
      .readOnly( false)
      .writeOnly( false)
      .notEnums( "Alpha", "Bravo", "Charlie")
      .maxLength( 19)
      .minLength( 11)
      .patterns( "[A-Z]*")
      .build();
    
    assertThat( "String schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( null)
      .readOnly( null)
      .writeOnly( true)
      .enums( "Alpha", "Bravo")
      .maxLength( 100)
      .minLength( 10)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( true)
      .writeOnly( null)
      .enums( "Bravo", "Charlie")
      .maxLength( null)
      .minLength( null)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( true)
      .readOnly( false)
      .writeOnly( true)
      .enums( "Alpha")
      .maxLength( 100)
      .minLength( 10)
      .notPatterns( "A.*", "B.*")
      .build();
    
    assertThat( "String schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> > baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .nullable( false)
      .readOnly( true)
      .writeOnly( false)
      .maxLength( null)
      .minLength( 10)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( true)
      .readOnly( null)
      .writeOnly( true)
      .maxLength( 11)
      .minLength( 100)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( true)
      .writeOnly( false)
      .maxLength( 99)
      .minLength( 12)
      .build();
    
    assertThat( "String schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Contains base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( null)
      .enums( "Alpha", "Bravo")
      .maxLength( null)
      .minLength( null)
      .patterns( "[A-Z]*")
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .nullable( null)
      .readOnly( false)
      .writeOnly( null)
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( null)
      .minLength( null)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( false)
      .readOnly( true)
      .writeOnly( null)
      .notEnums( "Alpha", "Bravo", "Charlie")
      .maxLength( null)
      .minLength( null)
      .patterns( "[A-Z]*")
      .notPatterns( "A.*", "B.*")
      .build();
    
    assertThat( "String schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> >= base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .nullable( true)
      .readOnly( false)
      .writeOnly( true)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( null)
      .patterns( "[A-Z]*")
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .nullable( null)
      .readOnly( null)
      .writeOnly( false)
      .maxLength( 10)
      .minLength( 1001)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .nullable( true)
      .readOnly( false)
      .writeOnly( true)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( 11)
      .patterns( "[A-Z]*")
      .build();
    
    assertThat( "String schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> <= baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( true)
      .readOnly( null)
      .writeOnly( false)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( 10)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( false)
      .enums( "Charlie")
      .maxLength( 5)
      .minLength( 100)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{writeOnly: false} is not consistent with {not: {writeOnly: false}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> <= baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> <FONT color="red"> Intersects base  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_9()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( true)
      .readOnly( null)
      .writeOnly( false)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( 10)
      .patterns( "[A-Z]*")
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( null)
      .enums( "Charlie")
      .maxLength( 5)
      .minLength( 100)
      .patterns( "[A-Z]*", "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{pattern: \"[A-Z]*\"} is not consistent with {not: {pattern: \"[A-Z]*\"}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> <= baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_10()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( false)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( 10)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( null)
      .enums( "Charlie")
      .maxLength( 5)
      .minLength( 100)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{nullable: false} is not consistent with {not: {nullable: false}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> <FONT color="red"> Includes-Base  </FONT> </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> <= baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_11()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( true)
      .readOnly( null)
      .writeOnly( false)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( 10)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "baseFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( null)
      .enums( "Charlie")
      .maxLength( 5)
      .minLength( 100)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{format: \"baseFormat\"} is not consistent with {not: {format: \"baseFormat\"}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> <= baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_12()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( true)
      .readOnly( true)
      .writeOnly( false)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( 10)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( true)
      .writeOnly( null)
      .enums( "Charlie")
      .maxLength( 5)
      .minLength( 100)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{readOnly: true} is not consistent with {not: {readOnly: true}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> <= baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_13()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( true)
      .readOnly( false)
      .writeOnly( false)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( 10)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( false)
      .writeOnly( null)
      .enums( "Charlie")
      .maxLength( 5)
      .minLength( 100)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{readOnly: false} is not consistent with {not: {readOnly: false}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> false </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> <= baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_14()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( true)
      .readOnly( null)
      .writeOnly( true)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( 10)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( false)
      .readOnly( null)
      .writeOnly( true)
      .enums( "Charlie")
      .maxLength( 5)
      .minLength( 100)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{writeOnly: true} is not consistent with {not: {writeOnly: true}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> <= baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_15()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( true)
      .readOnly( null)
      .writeOnly( false)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( 10)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( true)
      .readOnly( null)
      .writeOnly( null)
      .enums( "Charlie")
      .maxLength( 5)
      .minLength( 100)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "{nullable: true} is not consistent with {not: {nullable: true}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeStringSchemas mergeStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.nullable </TD> <TD> true </TD> </TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notFormats </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.nullable </TD> <TD> null </TD> </TR>
   * <TR><TD> not.readOnly </TD> <TD> <FONT color="red"> false  </FONT> </TD> </TR>
   * <TR><TD> not.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> not.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> not.maxLength </TD> <TD> <= baseMinLength </TD> </TR>
   * <TR><TD> not.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> not.patterns </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_16()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "baseFormat")
      .nullable( true)
      .readOnly( null)
      .writeOnly( true)
      .enums( "Alpha", "Bravo")
      .maxLength( 1000)
      .minLength( 10)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "string")
      .notFormats( "thisFormat", "thatFormat")
      .nullable( null)
      .readOnly( false)
      .writeOnly( null)
      .enums( "Charlie")
      .maxLength( 5)
      .minLength( 100)
      .patterns( "A.*", "B.*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    expectFailure( IllegalStateException.class)
      .when(() -> mergeSchemas( context, base, not))
      .then(
        failure ->
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't merge \"not\" schema requiring readOnly=true with base schema requiring writeOnly=true")));
    }
}

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
 * Runs tests for {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()}
 */
public class MergeArraySchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxItems </TD> <TD> > baseMinItems </TD> </TR>
   * <TR><TD> not.minItems </TD> <TD> < baseMaxItems </TD> </TR>
   * <TR><TD> not.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> not.items </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( false)
      .items( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "array")
      .maxItems( 11)
      .minItems( 99)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .maxItems( 98)
      .minItems( 12)
      .uniqueItems( false)
      .items( null)
      .notItems( SchemaBuilder.ofType( "number").build())
      .build();
    
    assertThat( "Array schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> not.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> not.items </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .maxItems( null)
      .minItems( null)
      .uniqueItems( null)
      .items( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "array")
      .maxItems( 11)
      .minItems( null)
      .uniqueItems( null)
      .items( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .maxItems( null)
      .minItems( 12)
      .uniqueItems( null)
      .items( null)
      .notItems( null)
      .build();
    
    assertThat( "Array schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maxItems </TD> <TD> > baseMinItems </TD> </TR>
   * <TR><TD> not.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> not.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> not.items </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .maxItems( null)
      .minItems( 10)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "array")
      .maxItems( 11)
      .minItems( null)
      .uniqueItems( null)
      .items( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .maxItems( null)
      .minItems( 12)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .notItems( null)
      .build();
    
    assertThat( "Array schema", merged, matches( new SchemaMatcher( expected)));
    }
  
  /**
   * Tests {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maxItems </TD> <TD> <= baseMinItems </TD> </TR>
   * <TR><TD> not.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> not.items </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .maxItems( null)
      .minItems( 10)
      .uniqueItems( null)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "array")
      .maxItems( 10)
      .minItems( 99)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "object").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .maxItems( 98)
      .minItems( 11)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "number").build())
      .notItems( SchemaBuilder.ofType( "object").build())
      .build();
    
    assertThat( "Array schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxItems </TD> <TD> <= baseMinItems </TD> </TR>
   * <TR><TD> not.minItems </TD> <TD> >= baseMaxItems </TD> </TR>
   * <TR><TD> not.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> not.items </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( null)
      .items( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "array")
      .maxItems( 10)
      .minItems( 100)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .maxItems( 99)
      .minItems( 11)
      .uniqueItems( true)
      .items( null)
      .notItems( SchemaBuilder.ofType( "number").build())
      .build();
    
    assertThat( "Array schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minItems </TD> <TD> < baseMaxItems </TD> </TR>
   * <TR><TD> not.uniqueItems </TD> <TD> null </TD> </TR>
   * <TR><TD> not.items </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( false)
      .items( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "array")
      .maxItems( 11)
      .minItems( 99)
      .uniqueItems( null)
      .items( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .maxItems( 98)
      .minItems( 12)
      .uniqueItems( false)
      .items( null)
      .notItems( null)
      .build();
    
    assertThat( "Array schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minItems </TD> <TD> >= baseMaxItems </TD> </TR>
   * <TR><TD> not.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> not.items </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .maxItems( 100)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "array")
      .maxItems( null)
      .minItems( 100)
      .uniqueItems( false)
      .items( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .maxItems( 99)
      .minItems( null)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
      .notItems( null)
      .build();
    
    assertThat( "Array schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> base.items </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maxItems </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> not.items </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .maxItems( null)
      .minItems( null)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "array")
      .maxItems( null)
      .minItems( 99)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "integer").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "array")
      .maxItems( 98)
      .minItems( null)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "number").build())
      .notItems( SchemaBuilder.ofType( "integer").build())
      .build();
    
    assertThat( "Array schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> false </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxItems </TD> <TD> > baseMinItems </TD> </TR>
   * <TR><TD> not.minItems </TD> <TD> < baseMaxItems </TD> </TR>
   * <TR><TD> not.uniqueItems </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.items </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( false)
      .items( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "array")
      .maxItems( 11)
      .minItems( 99)
      .uniqueItems( false)
      .items( SchemaBuilder.ofType( "number").build())
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
          is( "{uniqueItems: false} is not consistent with {not: {uniqueItems: false}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeArraySchemas mergeArraySchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minItems </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.uniqueItems </TD> <TD> true </TD> </TR>
   * <TR><TD> base.items </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maxItems </TD> <TD> > baseMinItems </TD> </TR>
   * <TR><TD> not.minItems </TD> <TD> < baseMaxItems </TD> </TR>
   * <TR><TD> not.uniqueItems </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.items </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_9()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .maxItems( 100)
      .minItems( 10)
      .uniqueItems( true)
      .items( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "array")
      .maxItems( 10)
      .minItems( 100)
      .uniqueItems( true)
      .items( SchemaBuilder.ofType( "number").build())
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
          is( "{uniqueItems: true} is not consistent with {not: {uniqueItems: true}}")));
    }
}

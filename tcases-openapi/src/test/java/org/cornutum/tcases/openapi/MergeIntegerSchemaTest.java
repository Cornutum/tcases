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
 * Runs tests for {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()}.
 */
public class MergeIntegerSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> < baseMinimum </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> < baseMaximum </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 100)
      .minimum( 10)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 5)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 9)
      .minimum( 99)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .notMultipleOfs( 3, 7)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 98)
      .minimum( 10)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( 5)
      .notMultipleOfs( 3, 7)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> > baseMaximum </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 7)
      .minimum( 100)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .notMultipleOfs( 5)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( 8)
      .exclusiveMaximum( false)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .notMultipleOfs( 5)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> > baseMinimum </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> < baseMaximum </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( 5)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 6)
      .minimum( 9)
      .exclusiveMaximum( null)
      .exclusiveMinimum( false)
      .notMultipleOfs( 3, 5)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 8)
      .minimum( 7)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .notMultipleOfs( 3, 5)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( 10)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .multipleOf( 5)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( 100)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .notMultipleOfs( 15)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 99)
      .minimum( 10)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .multipleOf( 5)
      .notMultipleOfs( 15)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> < baseMinimum </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( 10)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( 15)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 9)
      .minimum( 100)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 99)
      .minimum( 10)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( 15)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> > baseMinimum </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( 10)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .multipleOf( 15)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 11)
      .minimum( 15)
      .exclusiveMaximum( null)
      .exclusiveMinimum( false)
      .notMultipleOfs( 2, 7, 13)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 14)
      .minimum( 12)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .multipleOf( 15)
      .notMultipleOfs( 2, 7, 13)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( 12)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 11)
      .minimum( 11)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> < baseMinimum </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( 7)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 6)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .notMultipleOfs( 2)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( 7)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .notMultipleOfs( 2)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> < baseMaximum </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_9()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 100)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( 99)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .notMultipleOfs( 2, 3)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 98)
      .minimum( 11)
      .exclusiveMaximum( false)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .notMultipleOfs( 2, 3)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> > baseMinimum </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> > baseMaximum </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_10()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( 2)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 3)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 3)
      .minimum( 11)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .notMultipleOfs( 6)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( 4)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .multipleOf( 3)
      .notMultipleOfs( 6)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Merge (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> < baseMinimum </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> < baseMaximum </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_11()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( 2)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( 2)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 1)
      .minimum( 9)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> merged = mergeSchemas( context, base, not);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .maximum( 8)
      .minimum( 2)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( 2)
      .build();
    
    assertThat( "Integer schema", merged, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_12()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( 10)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .multipleOf( 5)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( 100)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .notMultipleOfs( 10, 20, 30)
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
          is( "{exclusiveMinimum: true} is not consistent with {not: {exclusiveMinimum: true}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 13. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_13()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .multipleOf( 3)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 3)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .notMultipleOfs( 15, 18)
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
          is( "{exclusiveMaximum: false} is not consistent with {not: {exclusiveMaximum: false}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 14. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> <FONT color="red"> Multiple of not  </FONT> </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_14()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .multipleOf( 15)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .notMultipleOfs( 3, 7)
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
          is( "{multipleOf: 15} is not consistent with {not: {multipleOf: 3}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 15. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_15()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( 10)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( 15)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( 2)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .notMultipleOfs( 2, 7)
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
          is( "{exclusiveMaximum: true} is not consistent with {not: {exclusiveMaximum: true}}")));
    }

  /**
   * Tests {@link SchemaUtils#mergeIntegerSchemas mergeIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 16. Merge (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> not.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> not.minimum </TD> <TD> Positive </TD> </TR>
   * <TR><TD> not.exclusiveMaximum </TD> <TD> (not applicable) </TD> </TR>
   * <TR><TD> not.exclusiveMinimum </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * <TR><TD> not.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Merge_16()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( 10)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( 3)
      .build();

    Schema<?> not =
      SchemaBuilder.ofType( "integer")
      .maximum( null)
      .minimum( 11)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .notMultipleOfs( 6, 60)
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
          is( "{exclusiveMinimum: false} is not consistent with {not: {exclusiveMinimum: false}}")));
    }
  }

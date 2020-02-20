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
 * Runs tests for {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()}.
 */
public class CombineNumberSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> float </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Contains base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> < base </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( null)
      .enumNumbers( 1.2, 2.4, 3.6)
      .maximum( 3.6)
      .minimum( 1.2)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( 1.2)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( 0.0, 1.2, 2.4, 3.6, 4.8)
      .maximum( null)
      .minimum( 0.0)
      .exclusiveMaximum( null)
      .exclusiveMinimum( false)
      .multipleOf( 0.6)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( 1.2, 2.4, 3.6)
      .maximum( 3.6)
      .exclusiveMaximum( true)
      .minimum( 1.2)
      .exclusiveMinimum( false)
      .multipleOf( 1.2)
      .build();
    
    assertThat( "Number schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> float </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> double </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( 0.00, 0.12, 0.24, 0.36)
      .maximum( 1.00)
      .minimum( 0.00)
      .exclusiveMaximum( false)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( "double")
      .enumNumbers( 0.24, 0.36, 0.48)
      .maximum( 2.00)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( 0.24, 0.36)
      .maximum( 1.00)
      .exclusiveMaximum( false)
      .minimum( 0.00)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .build();
    
    assertThat( "Number schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> double </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> float </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Subset of base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> > base </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( "double")
      .enumNumbers( 0.24, 0.36, 0.48)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 0.06)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( 0.24, 0.36)
      .maximum( null)
      .minimum( 0.24)
      .exclusiveMaximum( null)
      .exclusiveMinimum( false)
      .multipleOf( 0.12)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( 0.24, 0.36)
      .maximum( null)
      .exclusiveMaximum( null)
      .minimum( 0.24)
      .exclusiveMinimum( false)
      .multipleOf( 0.12)
      .build();
    
    assertThat( "Number schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notEnums </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notEnums </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( null)
      .maximum( 1.234)
      .minimum( -1.234)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .notEnumNumbers( 1.2, 3.4, 5)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( null)
      .maximum( 0.9)
      .minimum( -1.0)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( 0.10)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "number")
      .format( null)
      .maximum( 0.9)
      .exclusiveMaximum( true)
      .minimum( -1.0)
      .exclusiveMinimum( false)
      .multipleOf( 0.10)
      .notEnumNumbers( 1.2, 3.4, 5)
      .build();
    
    assertThat( "Number schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> double </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> double </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( "double")
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 0.01)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( "double")
      .enumNumbers( -1.1, 0.1, 1.1)
      .maximum( 1.1)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "number")
      .format( "double")
      .enumNumbers( -1.1, 0.1, 1.1)
      .maximum( 1.1)
      .exclusiveMaximum( true)
      .minimum( null)
      .exclusiveMinimum( null)
      .multipleOf( 0.01)
      .build();
    
    assertThat( "Number schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> double </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false</TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> < base </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( "double")
      .enumNumbers( -1.2, -0.9, -0.6, -0.3, 0.0)
      .maximum( 0.0)
      .minimum( -2.0)
      .exclusiveMaximum( false)
      .exclusiveMinimum( false)
      .multipleOf( 0.3)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( null)
      .maximum( null)
      .minimum( -3.0)
      .exclusiveMaximum( null)
      .exclusiveMinimum( false)
      .multipleOf( 0.1)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "number")
      .format( "double")
      .enumNumbers( -1.2, -0.9, -0.6, -0.3, 0.0)
      .maximum( 0.0)
      .exclusiveMaximum( false)
      .minimum( -2.0)
      .exclusiveMinimum( false)
      .multipleOf( 0.3)
      .build();
    
    assertThat( "Number schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> double </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Contains base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( null)
      .enumNumbers( -1.2, -0.9, -0.6, -0.3, 0.0)
      .maximum( 1.0)
      .minimum( -2.0)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( "double")
      .enumNumbers( -1.5, -1.2, -0.9, -0.6, -0.3, 0.0, 0.3)
      .maximum( 2.0)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "number")
      .format( "double")
      .enumNumbers( -1.2, -0.9, -0.6, -0.3, 0.0)
      .maximum( 1.0)
      .exclusiveMaximum( true)
      .minimum( -2.0)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();
    
    assertThat( "Number schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> float </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> > base </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( -1.2, -0.9, -0.6, -0.3, 0.0)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 0.1)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( null)
      .enumNumbers( -0.3, 0.0, 0.3)
      .maximum( null)
      .minimum( -1.0)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .multipleOf( 0.3)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( -0.3, 0.0)
      .maximum( null)
      .exclusiveMaximum( false)
      .minimum( -1.0)
      .exclusiveMinimum( true)
      .multipleOf( 0.3)
      .build();
    
    assertThat( "Number schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> float </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> float </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Subset of base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( -30, -20, -10, 0, 10, 20, 30)
      .maximum( 30)
      .minimum( -30)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( -10, 0, 10)
      .maximum( 10)
      .minimum( -10)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .multipleOf( 0.5)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( -10, 0, 10)
      .maximum( 10)
      .exclusiveMaximum( false)
      .minimum( -10)
      .exclusiveMinimum( true)
      .multipleOf( 0.5)
      .build();
    
    assertThat( "Number schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> float </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> <FONT color="red"> Disjoint from base  </FONT> </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_9()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( null)
      .enumNumbers( -10.5, 0, 10.5)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 0.5)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .enumNumbers( 1, 2, 3, 4)
      .maximum( 5)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {enum: [1.0, 2.0, 3.0, 4.0]} with schema requiring {enum: [-10.5, 0.0, 10.5]}"));
        });
    }

  /**
   * Tests {@link SchemaUtils#combineNumberSchemas combineNumberSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> float </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> <FONT color="red"> Incongruent  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_10()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "number")
      .format( null)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 0.5)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .format( "float")
      .maximum( 9.5)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 1.3)
      .build();

    NotificationContext context = new NotificationContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {multipleOf: 1.3} with schema requiring {multipleOf: 0.5}"));
        });
    }
  }

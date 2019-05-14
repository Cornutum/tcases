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
 * Runs tests for {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()}.
 */
public class CombineIntegerSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> int32 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> > base </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .format( null)
      .enums( 1, 2, 3)
      .maximum( 10)
      .exclusiveMaximum( true)
      .minimum( null)
      .exclusiveMinimum( null)
      .multipleOf( 2)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .maximum( null)
      .exclusiveMaximum( false)
      .minimum( 1)
      .exclusiveMinimum( false)
      .multipleOf( 4)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .enums( 1, 2, 3)
      .maximum( 10)
      .exclusiveMaximum( true)
      .minimum( 1)
      .exclusiveMinimum( false)
      .multipleOf( 4)
      .build();
    
    assertThat( "Integer schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> int32 </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> int32 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .maximum( 123)
      .minimum( 45)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .enums( 1, 12, 123)
      .maximum( 124)
      .minimum( 46)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .enums( 1, 12, 123)
      .maximum( 123)
      .exclusiveMaximum( true)
      .minimum( 46)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .build();
    
    assertThat( "Integer schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> int64 </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> int32 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Subset of base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> < base </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .format( "int64")
      .enums( 0, 20, 200, 2000)
      .maximum( 2000)
      .minimum( 20)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .multipleOf( 20)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .enums( 20, 200)
      .maximum( 200)
      .minimum( 0)
      .exclusiveMaximum( null)
      .exclusiveMinimum( false)
      .multipleOf( 10)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .enums( 20, 200)
      .maximum( 200)
      .exclusiveMaximum( null)
      .minimum( 20)
      .exclusiveMinimum( true)
      .multipleOf( 20)
      .build();
    
    assertThat( "Integer schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> int64 </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> int64 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .format( "int64")
      .maximum( null)
      .minimum( 34)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( "int64")
      .maximum( 568)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .multipleOf( 2)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .format( "int64")
      .maximum( 568)
      .exclusiveMaximum( false)
      .minimum( 34)
      .exclusiveMinimum( true)
      .multipleOf( 2)
      .build();
    
    assertThat( "Integer schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> int32 </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> int64 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Contains base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
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
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .enums( 10, 20, 30)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 5)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( "int64")
      .enums( 0, 10, 20, 30, 40)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .enums( 10, 20, 30)
      .maximum( null)
      .exclusiveMaximum( null)
      .minimum( null)
      .exclusiveMinimum( null)
      .multipleOf( 5)
      .build();
    
    assertThat( "Integer schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> int64 </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> > base </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .format( "int64")
      .maximum( 314159)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .multipleOf( 3)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( null)
      .enums( 3, 314, 3141, 31415)
      .maximum( null)
      .minimum( 3)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .multipleOf( 27)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .format( "int64")
      .enums( 3, 314, 3141, 31415)
      .maximum( 314159)
      .exclusiveMaximum( false)
      .minimum( 3)
      .exclusiveMinimum( true)
      .multipleOf( 27)
      .build();
    
    assertThat( "Integer schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> int64 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .format( null)
      .enums( 12, 120, 240, 600)
      .maximum( 600)
      .minimum( 12)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( "int64")
      .enums( 240, 360, 480, 600)
      .maximum( 720)
      .minimum( 24)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .format( "int64")
      .enums( 240, 600)
      .maximum( 600)
      .exclusiveMaximum( false)
      .minimum( 24)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();
    
    assertThat( "Integer schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> int32 </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> < base </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .maximum( 75)
      .minimum( 15)
      .exclusiveMaximum( false)
      .exclusiveMinimum( false)
      .multipleOf( 15)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( null)
      .maximum( 60)
      .minimum( 0)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .multipleOf( 3)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .maximum( 60)
      .exclusiveMaximum( null)
      .minimum( 15)
      .exclusiveMinimum( false)
      .multipleOf( 15)
      .build();
    
    assertThat( "Integer schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.multipleOf </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Subset of base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .format( null)
      .enums( 10, 100, 1000)
      .maximum( null)
      .minimum( 10)
      .exclusiveMaximum( null)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( null)
      .enums( 100)
      .maximum( 100)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .multipleOf( 10)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .format( null)
      .enums( 100)
      .maximum( 100)
      .exclusiveMaximum( true)
      .minimum( 10)
      .exclusiveMinimum( false)
      .multipleOf( 10)
      .build();
    
    assertThat( "Integer schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
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
   * <TR><TD> additional.format </TD> <TD> int32 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> <FONT color="red"> Disjoint from base  </FONT> </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
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
      SchemaBuilder.ofType( "integer")
      .format( null)
      .enums( 9, 27, 81)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 3)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .enums( 8, 24, 64)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> assertThat( "Failure", failure.getMessage(), is( "enum=[8, 24, 64] is not consistent with base enum=[9, 27, 81]")));
    }

  /**
   * Tests {@link SchemaUtils#combineIntegerSchemas combineIntegerSchemas()} using the following inputs.
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
   * <TR><TD> additional.format </TD> <TD> int32 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.multipleOf </TD> <TD> <FONT color="red"> Incongruent  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_10() {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .format( null)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 3)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .format( "int32")
      .enums( 8, 24, 64)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .multipleOf( 4)
      .build();

    OpenApiContext context = new OpenApiContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> assertThat( "Failure", failure.getMessage(), is( "multipleOf=4 is not consistent with base multipleOf=3")));
    }
  }

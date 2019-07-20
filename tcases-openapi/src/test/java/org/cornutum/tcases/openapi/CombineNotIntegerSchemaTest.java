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
 * Runs tests for {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()}.
 */
public class CombineNotIntegerSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> 0 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums()
      .maximum( 100)
      .minimum( 10)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( 20, 200, 2000)
      .maximum( 200)
      .minimum( 20)
      .exclusiveMaximum( null)
      .exclusiveMinimum( false)
      .multipleOf( 5)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .enums( 20, 200, 2000)
      .maximum( 200)
      .minimum( 10)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .notMultipleOfs( 5)
      .build();
    
    assertThat( "Integer not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> 0 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 10, 100)
      .maximum( 100)
      .minimum( 0)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .notMultipleOfs( 3, 7)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( -10, 1)
      .maximum( 10)
      .minimum( -100)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .enums( -10, 0, 1, 10, 100)
      .maximum( 100)
      .minimum( -100)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .notMultipleOfs( 3, 7)
      .build();
    
    assertThat( "Integer not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> 0 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums()
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums()
      .maximum( null)
      .minimum( 0)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .notMultipleOfs( 2, 3, 4)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .enums()
      .maximum( null)
      .minimum( 0)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .notMultipleOfs( 2, 3, 4)
      .build();
    
    assertThat( "Integer not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 20, 200, 2000)
      .maximum( 10)
      .minimum( 1)
      .exclusiveMaximum( false)
      .exclusiveMinimum( false)
      .multipleOf( 5)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( -100, 0, 100)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .notMultipleOfs( 2, 3, 7)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .enums( -100, 0, 20, 100, 200, 2000)
      .maximum( 10)
      .minimum( 1)
      .exclusiveMaximum( false)
      .exclusiveMinimum( false)
      .notMultipleOfs( 2, 3, 5, 7)
      .build();
    
    assertThat( "Integer not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums()
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .notMultipleOfs( 2, 3)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( -100, 0, 100)
      .maximum( 100)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .multipleOf( 4)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .enums( -100, 0, 100)
      .maximum( 100)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .notMultipleOfs( 2, 3)
      .build();
    
    assertThat( "Integer not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> 0 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 20, 200, 2000)
      .maximum( 2000)
      .minimum( 0)
      .exclusiveMaximum( false)
      .exclusiveMinimum( false)
      .multipleOf( 5)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( -1, 1)
      .maximum( 3000)
      .minimum( 20)
      .exclusiveMaximum( false)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .enums( -1, 0, 1, 20, 200, 2000)
      .maximum( 3000)
      .minimum( 0)
      .exclusiveMaximum( false)
      .exclusiveMinimum( false)
      .notMultipleOfs( 5)
      .build();
    
    assertThat( "Integer not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums()
      .maximum( 10)
      .minimum( 1)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .notMultipleOfs( 2, 3, 7, 15)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums()
      .maximum( 1)
      .minimum( -1)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .notMultipleOfs( 4, 5, 11)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .enums()
      .maximum( 10)
      .minimum( -1)
      .exclusiveMaximum( false)
      .exclusiveMinimum( true)
      .notMultipleOfs( 2, 3, 5, 7, 11)
      .build();
    
    assertThat( "Integer not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> 0 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> 0 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 20, 200, 2000)
      .maximum( 10)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( 100, 200, 300)
      .maximum( 300)
      .minimum( -1)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 20, 100, 200, 300, 2000)
      .maximum( 300)
      .minimum( -1)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .notMultipleOfs()
      .build();
    
    assertThat( "Integer not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums()
      .maximum( null)
      .minimum( 10)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .multipleOf( 5)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 20, 200, 2000)
      .maximum( null)
      .minimum( null)
      .exclusiveMaximum( null)
      .exclusiveMinimum( null)
      .multipleOf( 7)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 20, 200, 2000)
      .maximum( null)
      .minimum( 10)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .notMultipleOfs( 5, 7)
      .build();
    
    assertThat( "Integer not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> 0 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_9()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 20, 200, 2000)
      .maximum( 10)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( -20, -200, -2000)
      .maximum( 1)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .multipleOf( 15)
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
          is( "Can't combine schema requiring {not: {exclusiveMaximum: true}} with base schema requiring {not: {exclusiveMaximum: false}}")));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> 0 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_10()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 20, 200, 2000)
      .maximum( 10)
      .minimum( null)
      .exclusiveMaximum( true)
      .exclusiveMinimum( null)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( -20, -200, -2000)
      .maximum( 1)
      .minimum( null)
      .exclusiveMaximum( false)
      .exclusiveMinimum( null)
      .multipleOf( 15)
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
          is( "Can't combine schema requiring {not: {exclusiveMaximum: false}} with base schema requiring {not: {exclusiveMaximum: true}}")));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 11. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> <FONT color="red"> falseInconsistent  </FONT> </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> 0 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_11()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 20, 200, 2000)
      .maximum( 10)
      .minimum( 1)
      .exclusiveMaximum( true)
      .exclusiveMinimum( false)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( -20, -200, -2000)
      .maximum( null)
      .minimum( 20)
      .exclusiveMaximum( null)
      .exclusiveMinimum( true)
      .multipleOf( 15)
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
          is( "Can't combine schema requiring {not: {exclusiveMinimum: true}} with base schema requiring {not: {exclusiveMinimum: false}}")));
    }

  /**
   * Tests {@link SchemaUtils#combineNotIntegerSchemas combineNotIntegerSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 12. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maximum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minimum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.exclusiveMaximum </TD> <TD> true </TD> </TR>
   * <TR><TD> base.exclusiveMinimum </TD> <TD> <FONT color="red"> trueInconsistent  </FONT> </TD> </TR>
   * <TR><TD> base.notMultipleOfs </TD> <TD> 0 </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Disjoint from base </TD> </TR>
   * <TR><TD> additional.maximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minimum </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.exclusiveMaximum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.exclusiveMinimum </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.notMultipleOfs </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_12()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .enums( 0, 20, 200, 2000)
      .maximum( 10)
      .minimum( 0)
      .exclusiveMaximum( true)
      .exclusiveMinimum( true)
      .multipleOf( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .enums( -20, -200, -2000)
      .maximum( null)
      .minimum( 200)
      .exclusiveMaximum( null)
      .exclusiveMinimum( false)
      .multipleOf( 15)
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
          is( "Can't combine schema requiring {not: {exclusiveMinimum: false}} with base schema requiring {not: {exclusiveMinimum: true}}")));
    }
  }

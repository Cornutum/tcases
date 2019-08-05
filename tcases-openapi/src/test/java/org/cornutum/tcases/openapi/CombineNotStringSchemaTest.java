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
import static org.hamcrest.MatcherAssert.*;

/**
 * Runs tests for {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()}.
 */
public class CombineNotStringSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( 128)
      .minLength( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( 1)
      .patterns( "[0-9]*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .maxLength( 128)
      .minLength( 1)
      .patterns( "[0-9]*")
      .build();
    
    assertThat( "String not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> 1 </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( 128)
      .minLength( null)
      .patterns( "[A-Z]*")
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( 256)
      .minLength( null)
      .patterns( "[0-9]*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .maxLength( 256)
      .minLength( null)
      .patterns( "[A-Z]*", "[0-9]*")
      .build();
    
    assertThat( "String not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( 128)
      .minLength( 2)
      .patterns( "[A-Z]*", "[0-9]+")
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( 127)
      .minLength( null)
      .patterns( "A+B+")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .maxLength( 128)
      .minLength( 2)
      .patterns( "[A-Z]*", "[0-9]+", "A+B+")
      .build();
    
    assertThat( "String not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> 1 </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( 10)
      .patterns( "[A-Z]*")
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( 20)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( 10)
      .patterns( "[A-Z]*")
      .build();
    
    assertThat( "String not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> 1 </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( 10)
      .patterns( "[A-Z]*")
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( 256)
      .minLength( 1)
      .patterns( "[A-Z]*", "[0-9]*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .maxLength( 256)
      .minLength( 1)
      .patterns( "[A-Z]*", "[0-9]*")
      .build();
    
    assertThat( "String not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( 128)
      .minLength( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( 1)
      .patterns( "[0-9]*", "A+B+")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .maxLength( 128)
      .minLength( 1)
      .patterns( "A+B+", "[0-9]*")
      .build();
    
    assertThat( "String not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( 128)
      .minLength( null)
      .patterns( "[A-Z]*", "[0-9]*", "A+B+")
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( 256)
      .minLength( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .maxLength( 256)
      .minLength( null)
      .patterns( "[A-Z]*", "[0-9]*", "A+B+")
      .build();
    
    assertThat( "String not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( 128)
      .minLength( 16)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( 64)
      .minLength( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .maxLength( 128)
      .minLength( 16)
      .build();
    
    assertThat( "String not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Positive </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> > 1 </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> > 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( 16)
      .patterns( "[A-Z]*", "A+B+")
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( 64)
      .patterns( "[0-9]*", "X", "A+B+")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    Schema<?> combined = combineNotSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( 16)
      .patterns( "[0-9]*", "X", "A+B+", "[A-Z]*")
      .build();
    
    assertThat( "String not schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 9. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> <FONT color="red"> 0  </FONT> </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_9()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( null)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( 256)
      .minLength( 0)
      .patterns( "[0-9]*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing ",
      "\"not: {minLength: 0}\" assertion can't be satisfied by any instance");
    }

  /**
   * Tests {@link SchemaUtils#combineNotStringSchemas combineNotStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> <FONT color="red"> 0  </FONT> </TD> </TR>
   * <TR><TD> base.patterns </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> Positive </TD> </TR>
   * <TR><TD> additional.patterns </TD> <TD> 1 </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_10()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .maxLength( null)
      .minLength( 0)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .maxLength( 256)
      .minLength( 1)
      .patterns( "[0-9]*")
      .build();

    NotificationContext context = new NotificationContext();
    
    // When...
    assertOpenApiException(
      () -> combineNotSchemas( context, base, additional),
      "Error processing ",
      "\"not: {minLength: 0}\" assertion can't be satisfied by any instance");
    }
  }

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
 * Runs tests for {@link SchemaUtils#combineStringSchemas combineStringSchemas()}.
 */
public class CombineStringSchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#combineStringSchemas combineStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notEnums </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.pattern </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notEnums </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.pattern </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "date")
      .maxLength( 128)
      .minLength( null)
      .patterns( "[A-Z]*")
      .notEnums( "X", "Y", "Z", "Z")
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( null)
      .maxLength( 256)
      .minLength( 1)
      .patterns( "[0-9]*")
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "date")
      .maxLength( 128)
      .minLength( 1)
      .patterns( "[A-Z]*", "[0-9]*")
      .notEnums( "X", "Y", "Z")
      .build();
    
    assertThat( "String schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineStringSchemas combineStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.pattern </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Same as base </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Intersects base </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.pattern </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "date-time")
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 128)
      .minLength( 16)
      .patterns( ".*")
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( "date-time")
      .enums( "Charlie", "Delta", "Easy")
      .maxLength( null)
      .minLength( 32)
      .patterns()
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "date-time")
      .enums( "Charlie")
      .maxLength( 128)
      .minLength( 32)
      .patterns( ".*")
      .build();
    
    assertThat( "String schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineStringSchemas combineStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.pattern </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Subset of base </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.pattern </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( null)
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( null)
      .minLength( 8)
      .patterns()
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( null)
      .enums( "Alpha", "Charlie")
      .maxLength( null)
      .minLength( null)
      .patterns( "[A-Z]*")
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( null)
      .enums( "Alpha", "Charlie")
      .maxLength( null)
      .minLength( 8)
      .patterns( "[A-Z]*")
      .build();
    
    assertThat( "String schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineStringSchemas combineStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.pattern </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notPatterns </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.pattern </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notPatterns </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "byte")
      .maxLength( 128)
      .minLength( null)
      .patterns()
      .notPatterns( "A", "B", "B", "C")
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( null)
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 16)
      .minLength( null)
      .patterns()
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "byte")
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 16)
      .minLength( null)
      .patterns()
      .notPatterns( "A", "B", "C")
      .build();
    
    assertThat( "String schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineStringSchemas combineStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.pattern </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> Same as base </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> < base </TD> </TR>
   * <TR><TD> additional.pattern </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "email")
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( null)
      .minLength( 8)
      .patterns( "[A-Z]*")
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( "email")
      .maxLength( 128)
      .minLength( 1)
      .patterns( ".*")
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "email")
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 128)
      .minLength( 8)
      .patterns( "[A-Z]*", ".*")
      .build();
    
    assertThat( "String schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineStringSchemas combineStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> null </TD> </TR>
   * <TR><TD> base.pattern </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> Contains base </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.pattern </TD> <TD> null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( null)
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 128)
      .minLength( null)
      .patterns( ".*")
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( null)
      .enums( "Alpha", "Bravo", "Charlie", "Delta")
      .maxLength( 256)
      .minLength( 1)
      .patterns()
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( null)
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 128)
      .minLength( 1)
      .patterns( ".*")
      .build();
    
    assertThat( "String schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineStringSchemas combineStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> base.notEnums </TD> <TD> null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.pattern </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> <FONT color="red"> Different from base  </FONT> </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.notEnums </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.pattern </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "byte")
      .maxLength( 128)
      .minLength( 16)
      .patterns()
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( "date")
      .maxLength( null)
      .minLength( 32)
      .patterns( "[A-Z]*")
      .notEnums( "Q", "K", "K")
      .build();

    NotificationContext context = new NotificationContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {format: date} with schema requiring {format: byte}"));
        });
    }

  /**
   * Tests {@link SchemaUtils#combineStringSchemas combineStringSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.format </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.enum </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.maxLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.minLength </TD> <TD> Non-null </TD> </TR>
   * <TR><TD> base.pattern </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.format </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.enum </TD> <TD> <FONT color="red"> Disjoint from base  </FONT> </TD> </TR>
   * <TR><TD> additional.maxLength </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.minLength </TD> <TD> > base </TD> </TR>
   * <TR><TD> additional.pattern </TD> <TD> Non-null </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "password")
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 128)
      .minLength( 127)
      .patterns()
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( null)
      .enums( "Delta", "Easy", "Foxtrot")
      .maxLength( null)
      .minLength( 128)
      .patterns( "[A-Z]*")
      .build();

    NotificationContext context = new NotificationContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {enum: [Delta, Easy, Foxtrot]} with schema requiring {enum: [Alpha, Bravo, Charlie]}"));
        });
    }
  
  @Test
  public void whenNotEnumsCombined()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "date")
      .maxLength( 128)
      .minLength( null)
      .patterns( "[A-Z]*")
      .notEnums( "X", "Y", "Z", "Z")
      .build();

    NotificationContext context = new NotificationContext();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( null)
      .maxLength( 256)
      .minLength( 1)
      .patterns( "[0-9]*")
      .notEnums( "A", "B", "Z")
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "date")
      .maxLength( 128)
      .minLength( 1)
      .patterns( "[A-Z]*", "[0-9]*")
      .notEnums( "A", "B", "X", "Y", "Z")
      .build();
    
    assertThat( "String schema", combined, matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenNotEnumsConsistent()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "date-time")
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 128)
      .minLength( 16)
      .patterns( ".*")
      .build();

    NotificationContext context = new NotificationContext();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( "date-time")
      .enums( "Charlie", "Delta", "Easy")
      .maxLength( null)
      .minLength( 32)
      .patterns()
      .notEnums( "Delta", "Easy")
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "date-time")
      .enums( "Charlie")
      .maxLength( 128)
      .minLength( 32)
      .patterns( ".*")
      .notEnums( "Delta", "Easy")
      .build();
    
    assertThat( "String schema", combined, matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenNotEnumsInconsistent()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "email")
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( null)
      .minLength( 8)
      .patterns( "[A-Z]*")
      .build();

    NotificationContext context = new NotificationContext();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( "email")
      .maxLength( 128)
      .minLength( 1)
      .patterns( ".*")
      .notEnums( "Charlie")
      .build();

    // Then...
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {enum: Charlie} with schema requiring {not: {enum: Charlie}}"));
        });
    }
  
  @Test
  public void whenNotPatternsCombined()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "byte")
      .maxLength( 128)
      .minLength( null)
      .patterns()
      .notPatterns( "A", "B", "B", "C")
      .build();

    NotificationContext context = new NotificationContext();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( null)
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 16)
      .minLength( null)
      .patterns()
      .notPatterns( "A", "C", "E", "G")
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "byte")
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 16)
      .minLength( null)
      .patterns()
      .notPatterns( "A", "B", "C", "E", "G")
      .build();
    
    assertThat( "String schema", combined, matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenNotPatternsConsistent()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "date")
      .maxLength( 128)
      .minLength( null)
      .patterns( "[A-Z]*")
      .notEnums( "X", "Y", "Z", "Z")
      .build();

    NotificationContext context = new NotificationContext();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( null)
      .maxLength( 256)
      .minLength( 1)
      .patterns( "[0-9]*")
      .notPatterns( "X+")
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "string")
      .format( "date")
      .maxLength( 128)
      .minLength( 1)
      .patterns( "[A-Z]*", "[0-9]*")
      .notPatterns( "X+")
      .notEnums( "X", "Y", "Z")
      .build();
    
    assertThat( "String schema", combined, matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenNotPatternsInconsistent()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .format( "byte")
      .maxLength( 128)
      .minLength( null)
      .patterns()
      .notPatterns( "A", "B", "B", "C")
      .build();

    NotificationContext context = new NotificationContext();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .format( null)
      .enums( "Alpha", "Bravo", "Charlie")
      .maxLength( 16)
      .minLength( null)
      .patterns( "C", "D")
      .build();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {pattern: 'C'} with schema requiring {not: {pattern: 'C'}}"));
        });
    }
  }

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
 * Runs tests for {@link SchemaUtils#combineSchemas combineSchemas()} for
 * schemas designated as {@link SchemaUtils#isPropertySchema property schemas}.
 */
public class CombinePropertySchemaTest extends OpenApiTest
  {
  /**
   * Tests {@link SchemaUtils#combineSchemas combineSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 0. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_0()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.propertySchema( "integer")
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .readOnly( false)
      .writeOnly( true)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.propertySchema( "integer")
      .readOnly( false)
      .writeOnly( true)
      .build();
    
    assertThat( "Property schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineSchemas combineSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 1. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_1()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "string")
      .readOnly( false)
      .build();

    NotificationContext context = new NotificationContext();

    // When...
    Schema<?> notProperty = copySchema( base);

    // Then...
    assertThat( "Read only", notProperty.getReadOnly(), nullValue());

    // Given...
    Schema<?> additional =
      SchemaBuilder.propertySchema( "string")
      .readOnly( false)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.propertySchema( "string")
      .readOnly( false)
      .build();
    
    assertThat( "Property schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineSchemas combineSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 2. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_2()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.propertySchema( "object")
      .readOnly( true)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.propertySchema( "object")
      .readOnly( true)
      .writeOnly( false)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.propertySchema( "object")
      .readOnly( true)
      .writeOnly( false)
      .build();
    
    assertThat( "Property schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineSchemas combineSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 3. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_3()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "integer")
      .readOnly( false)
      .writeOnly( true)
      .build();

    NotificationContext context = new NotificationContext();

    // When...
    Schema<?> notProperty = copySchema( base);

    // Then...
    assertThat( "Read only", notProperty.getReadOnly(), nullValue());
    assertThat( "Write only", notProperty.getWriteOnly(), nullValue());

    // Given...

    Schema<?> additional =
      SchemaBuilder.propertySchema( "integer")
      .writeOnly( true)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.propertySchema( "integer")
      .readOnly( false)
      .writeOnly( true)
      .build();
    
    assertThat( "Property schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineSchemas combineSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 4. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_4()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.propertySchema( "string")
      .writeOnly( false)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .readOnly( true)
      .writeOnly( false)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.propertySchema( "string")
      .readOnly( true)
      .writeOnly( false)
      .build();
    
    assertThat( "Property schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineSchemas combineSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 5. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_5()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.propertySchema( "object")
      .readOnly( false)
      .writeOnly( true)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.propertySchema( "object")
      .readOnly( false)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.propertySchema( "object")
      .readOnly( false)
      .writeOnly( true)
      .build();
    
    assertThat( "Property schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineSchemas combineSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 6. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> false </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_6()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.propertySchema( "integer")
      .readOnly( true)
      .writeOnly( false)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "integer")
      .readOnly( true)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.propertySchema( "integer")
      .readOnly( true)
      .writeOnly( false)
      .build();
    
    assertThat( "Property schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineSchemas combineSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 7. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_7()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.propertySchema( "string")
      .readOnly( true)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.propertySchema( "string")
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.propertySchema( "string")
      .readOnly( true)
      .build();
    
    assertThat( "Property schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineSchemas combineSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 8. Combine (Success) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> Yes </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_8()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.propertySchema( "object")
      .writeOnly( true)
      .build();

    NotificationContext context = new NotificationContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( "object")
      .writeOnly( true)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.propertySchema( "object")
      .writeOnly( true)
      .build();
    
    assertThat( "Property schema", combined, matches( new SchemaMatcher( expected)));
    }

  /**
   * Tests {@link SchemaUtils#combineSchemas combineSchemas()} using the following inputs.
   * <P>
   * <TABLE border="1" cellpadding="8">
   * <TR align="left"><TH colspan=2> 10. Combine (<FONT color="red">Failure</FONT>) </TH></TR>
   * <TR align="left"><TH> Input Choice </TH> <TH> Value </TH></TR>
   * <TR><TD> base.readOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> base.writeOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.readOnly </TD> <TD> true </TD> </TR>
   * <TR><TD> additional.writeOnly </TD> <TD> null </TD> </TR>
   * <TR><TD> Consistent.ReadWriteOnly </TD> <TD> <FONT color="red"> No  </FONT> </TD> </TR>
   * </TABLE>
   * </P>
   */
  @Test
  public void Combine_10()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.propertySchema( "integer")
      .readOnly( null)
      .writeOnly( true)
      .build();

    Schema<?> additional =
      SchemaBuilder.propertySchema( "integer")
      .readOnly( true)
      .writeOnly( null)
      .build();

    NotificationContext context = new NotificationContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {readOnly: true} with schema requiring {writeOnly: true}"));
        });
    }

  @Test
  public void whenInconsistentReadOnly()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.propertySchema( "string")
      .readOnly( true)
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .readOnly( false)
      .build();

    NotificationContext context = new NotificationContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {readOnly: false} with schema requiring {readOnly: true}"));
        });
    }
  
  @Test
  public void whenInconsistentWriteOnly()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( "array")
      .writeOnly( false)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    Schema<?> additional =
      SchemaBuilder.propertySchema( "array")
      .writeOnly( true)
      .items( SchemaBuilder.ofType( "number").build())
      .build();

    NotificationContext context = new NotificationContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {writeOnly: true} with schema requiring {writeOnly: false}"));
        });
    }
  }

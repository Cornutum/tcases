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
 * Runs tests for {@link SchemaUtils#combineGenericSchemas combineGenericSchemas()}.
 */
public class CombineGenericSchemaTest extends OpenApiTest
  {
  @Test
  public void whenNotTypesCombined()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( null)
      .notTypes( "object", "string")
      .build();

    ModelConditionContext context = new ModelConditionContext();

    // Then...
    assertThat( "With empty", copySchema( base), matches( new SchemaMatcher( base)));
    assertThat( "With self", combineSchemas( context, base, base), matches( new SchemaMatcher( base)));

    // Given...
    Schema<?> additional =
      SchemaBuilder.ofType( null)
      .notTypes( "object", "boolean")
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( null)
      .notTypes( "object", "boolean", "string")
      .build();
    
    assertThat( "Generic schema", combined, matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenNotTypesConsistent()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( null)
      .notTypes( "object", "string")
      .build();

    ModelConditionContext context = new ModelConditionContext();

    Schema<?> additional =
      SchemaBuilder.ofType( "number")
      .maximum( 1.0)
      .minimum( -1.0)
      .build();

    // When...
    Schema<?> combined = combineSchemas( context, base, additional);

    // Then...
    Schema<?> expected =
      SchemaBuilder.ofType( "number")
      .maximum( 1.0)
      .minimum( -1.0)
      .build();
    
    assertThat( "Generic schema", combined, matches( new SchemaMatcher( expected)));
    }
  
  @Test
  public void whenNotTypesInconsistent()
    {
    // Given...
    Schema<?> base =
      SchemaBuilder.ofType( null)
      .notTypes( "boolean", "string")
      .build();

    Schema<?> additional =
      SchemaBuilder.ofType( "string")
      .build();

    ModelConditionContext context = new ModelConditionContext();
    
    expectFailure( IllegalStateException.class)
      .when( () -> combineSchemas( context, base, additional))
      .then( failure -> {
        assertThat(
          "Failure",
          failure.getMessage(),
          is( "Can't combine schema requiring {type: string} with schema requiring {not: {type: string}}"));
        });
    }
  }

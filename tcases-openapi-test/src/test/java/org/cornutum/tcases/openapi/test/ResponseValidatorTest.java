//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;

import java.util.function.Consumer;

/**
 * Base class for {@link ResponseValidator} tests.
 */
public abstract class ResponseValidatorTest
  {
  /**
   * Returns a validator for the {@link ResponsesDef} defined by the content of the given resource.
   */
  protected ResponseValidator validatorFor( String resource)
    {
    try
      {
      return new ResponseValidator( getClass().getResourceAsStream( String.format( "%s.json", resource)));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( String.format( "Can't read ResponsesDef from resource=%s", resource), e);
      }
    }
  
  /**
   * Returns a validator for the {@link ResponsesDef} defined by the content of the given resource, using
   * the given {@link ResponseUnvalidatedException} handler.
   */
  protected ResponseValidator validatorFor( String resource, Consumer<ResponseUnvalidatedException> unvalidatedHandler)
    {
    return validatorFor( resource).whenUnvalidated( unvalidatedHandler);
    }

  /**
   * Throws an AssertionError if the given failure message does not match the expected contents.
   */
  protected void assertValidationErrors( ResponseValidationException failure, String... expectedLines) 
    {
    String[] actualLines = failure.getMessage().split( "\\n", 0);
    assertThat(
      "Validation errors",
      actualLines,
      listsElements( expectedLines));
    }

  }

//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.hamcrest.Composites.*;
import static org.hamcrest.MatcherAssert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

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
    return new ResponseValidator( getClass(), String.format( "%s.json", resource));
    }
  
  /**
   * Returns a validator for the {@link ResponsesDef} defined by the content of the given resource, using
   * the given {@link ResponseValidationHandler}.
   */
  protected ResponseValidator validatorFor( String resource, ResponseValidationHandler validationHandler)
    {
    return validatorFor( resource).notifying( validationHandler);
    }

  /**
   * Throws an AssertionError if the given failure message does not match the expected contents.
   */
  protected void assertValidationErrors( Exception failure, String... expectedLines) 
    {
    List<String> actualLines = new ArrayList<String>();
    for( Throwable cause = failure; cause != null; cause = cause.getCause())
      {
      actualLines.addAll( Arrays.asList( cause.getMessage().split( "\\n", 0)));
      }

    assertThat(
      "Validation errors",
      actualLines,
      listsMembers( expectedLines));
    }

  }

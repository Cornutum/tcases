//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.reader;

import org.cornutum.tcases.openapi.OpenApiException;

import java.net.URL;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.stream.Collectors.joining;

/**
 * Reports an error reading an OpenApi document.
 */
public class OpenApiReaderException extends OpenApiException
  {  
  private static final long serialVersionUID = 4942175897026283955L;

  /**
   * Creates a new OpenApiReaderException instance.
   */
  public OpenApiReaderException( URL location, Throwable cause)
    {
    super( basicReasonFor( location), cause);
    }

  /**
   * Creates a new OpenApiReaderException instance.
   */
  public OpenApiReaderException( URL location, List<String> errors)
    {
    super( errorReasonFor( location, errors));
    errors_ = errors;
    }

  /**
   * If this exception was caused by OpenAPI conformance errors, returns the list of errors.
   * Otherwise, returns null.
   */
  public List<String> getErrors()
    {
    return errors_;
    }

  /**
   * Returns a basic exception reason for the document at the given location.
   */
  private static String basicReasonFor( URL location)
    {
    return String.format( "Can't read Open API document%s", locationId( location));
    }

  /**
   * Returns a reason for the errors in the document at the given location.
   */
  private static String errorReasonFor( URL location, List<String> errors)
    {
    return
      Stream.concat(
        Stream.of( String.format( "%s conformance problem(s) found in Open API document%s", errors.size(), locationId( location))),
        IntStream.range( 0, errors.size()).mapToObj( i -> String.format( "[%s] %s", i, errors.get(i))))
      .collect( joining( "\n"));
    }

  /**
   * Returns a string identifying the given location
   */
  private static String locationId( URL location)
    {
    return
      location == null
      ? ""
      : String.format( "=%s", location);
    }

  private List<String> errors_;
  }

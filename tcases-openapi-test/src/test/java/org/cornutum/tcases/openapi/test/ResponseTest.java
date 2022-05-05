//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.CollectionUtils.*;
import static org.cornutum.tcases.openapi.test.JsonUtils.*;

import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.FileReader;
import java.io.Reader;
import java.io.StringReader;

/**
 * Base class for response tests
 */
public abstract class ResponseTest
  {
  /**
   * Reads response definitions from a resource file.
   */
  protected ResponsesDef readResponses( String resource)
    {
    try( FileReader reader = new FileReader( getClass().getResource( resource + ".json").getFile()))
      {
      return ResponsesDef.read( reader);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't read ResponsesDef", e);
      }
    }

  /**
   * Returns the JSON object represented by the given string.
   */
  protected JsonNode toJson( String json)
    {
    try( StringReader reader = new StringReader( json))
      {
      return toJson( reader);
      }
    }

  /**
   * Returns the JSON object represented by the given string.
   */
  protected JsonNode toJson( Reader reader)
    {
    try
      {
      return mapper().readTree( reader);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't read JSON value", e);
      }
    }

  /**
   * Returns if the given schema specifies that the property defined at the given location is "required".
   */
  protected boolean isRequired( JsonNode schema, JsonPointer propertyLocation)
    {
    String property = tailOf( propertyLocation);
    JsonPointer propertiesLocation = propertyLocation.head();
    ObjectNode objectSchema = expectObject( schema.at( propertiesLocation.head()));

    return
      asArray( objectSchema.get( "required"))
      .map( required -> toStream( required.elements()).anyMatch( e -> property.equals( e.asText())))
      .orElse( false);
    }

  }

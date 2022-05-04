//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import static org.cornutum.tcases.openapi.test.JsonUtils.mapper;

import com.fasterxml.jackson.databind.JsonNode;

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
  }

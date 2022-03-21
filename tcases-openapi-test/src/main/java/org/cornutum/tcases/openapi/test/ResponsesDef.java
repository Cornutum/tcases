//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.test;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.io.Reader;
import java.io.Writer;
import java.util.Objects;
import java.util.Optional;

/**
 * Defines the responses for requests described by an OpenAPI definition.
 */
public class ResponsesDef
  {
  /**
   * Creates a new ResponsesDef instance.
   */
  public ResponsesDef( ObjectNode root)
    {
    root_ = root;
    }

  /**
   * Writes a JSON representation of response definitions to the given output stream.
   */
  public static void write( ResponsesDef responses, Writer writer)
    {
    ObjectMapper mapper = new ObjectMapper();
    try( JsonGenerator generator = mapper.writerWithDefaultPrettyPrinter().createGenerator( writer))
      {
      mapper.writeTree( generator, responses.root_);
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't write JSON document", e);
      }
    }

  /**
   * Reads a JSON representation of response definitions from the given input stream.
   */
  public static ResponsesDef read( Reader reader)
    {
    try
      {
      ObjectMapper mapper = new ObjectMapper();
      return
        new ResponsesDef(
          Optional.of( mapper.readTree( reader))
          .filter( JsonNode::isObject)
          .map( root -> (ObjectNode) root)
          .orElseThrow( () -> new IllegalStateException( "Expected JSON type=object")));
      }
    catch( Exception e)
      {
      throw new IllegalStateException( "Can't read JSON document", e);
      }
    }

  @Override
  public String toString()
    {
    return String.valueOf( root_);
    }

  @Override
  public boolean equals( Object object)
    {
    ResponsesDef other =
      object != null && object.getClass().equals( getClass())?
      (ResponsesDef) object :
      null;

    return
      other != null
      && Objects.equals( other.root_, root_);
    }

  @Override
  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( root_);
    }
  
  private final ObjectNode root_;
  }

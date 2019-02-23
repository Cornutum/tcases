//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.SystemInputDef;
import org.cornutum.tcases.SystemTestDef;
import org.cornutum.tcases.generator.IGeneratorSet;
import org.cornutum.tcases.generator.io.GeneratorSetJson;

import java.net.URI;
import java.util.Optional;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;
import static javax.json.JsonValue.ValueType.*;

/**
 * Converts between a {@link Project} and its corresponding {@link JsonObject}.
 */
public final class ProjectJson
  {
  private ProjectJson()
    {
    // Static methods only
    }

  /**
   * Returns the Project represented by the given JSON object.
   */
  public static Project asProject( JsonObject json)
    {
    return
      ProjectBuilder.with( asSystemInputDef( json.get( INPUTDEF_KEY)))
      .systemInputRef( asSystemInputRef( json.get( INPUTDEF_KEY)))  

      .generators( asGeneratorSet( json.get( GENERATORS_KEY)))
      .generatorsRef( asGeneratorSetRef( json.get( GENERATORS_KEY)))

      .baseTests( asSystemTestDef( json.get( BASETESTS_KEY)))
      .baseTestRef( asSystemTestRef( json.get( BASETESTS_KEY)))

      .refBase( asRefBase( json.getJsonString( REFBASE_KEY)))
      .build();
    }

  /**
   * Returns the reference base URI represented by the given JSON string.
   */
  private static URI asRefBase( JsonString uri)
    {
    try
      {
      return
        uri == null
        ? null
        : new URI( uri.getChars().toString());
      }
    catch( Exception e)
      {
      throw new ProjectException( String.format( "Error defining reference base=%s", uri), e);
      }
    }

  /**
   * Returns the system input definition represented by the given JSON value.
   */
  private static SystemInputDef asSystemInputDef( JsonValue json)
    {
    try
      {
      SystemInputDef systemInput = null;
      if( json != null && json.getValueType() == OBJECT)
        {
        systemInput = SystemInputJson.asSystemInputDef( (JsonObject) json);
        }

      return systemInput;
      }
    catch( Exception e)
      {
      throw new ProjectException( "Error reading input definition", e);
      }
    }

  /**
   * Returns the system input definition URL represented by the given JSON value.
   */
  private static URI asSystemInputRef( JsonValue json)
    {
    try
      {
      URI uri = null;
      if( json != null && json.getValueType() == STRING)
        {
        uri = new URI( ((JsonString) json).getChars().toString());
        }

      return uri;
      }
    catch( Exception e)
      {
      throw new ProjectException( "Error reading input definition", e);
      }
    }

  /**
   * Returns the generator set represented by the given JSON value.
   */
  private static IGeneratorSet asGeneratorSet( JsonValue json)
    {
    try
      {
      IGeneratorSet generators = null;
      if( json != null && json.getValueType() == OBJECT)
        {
        generators = GeneratorSetJson.asGeneratorSet( (JsonObject) json);
        }

      return generators;
      }
    catch( Exception e)
      {
      throw new ProjectException( "Error reading generators", e);
      }
    }

  /**
   * Returns the generator set URL represented by the given JSON value.
   */
  private static URI asGeneratorSetRef( JsonValue json)
    {
    try
      {
      URI uri = null;
      if( json != null && json.getValueType() == STRING)
        {
        uri = new URI( ((JsonString) json).getChars().toString());
        }

      return uri;
      }
    catch( Exception e)
      {
      throw new ProjectException( "Error reading generators", e);
      }
    }

  /**
   * Returns the base test definition represented by the given JSON value.
   */
  private static SystemTestDef asSystemTestDef( JsonValue json)
    {
    try
      {
      SystemTestDef systemTest = null;
      if( json != null && json.getValueType() == OBJECT)
        {
        systemTest = SystemTestJson.asSystemTestDef( (JsonObject) json);
        }

      return systemTest;
      }
    catch( Exception e)
      {
      throw new ProjectException( "Error reading base tests definition", e);
      }
    }

  /**
   * Returns the base test definition URL represented by the given JSON value.
   */
  private static URI asSystemTestRef( JsonValue json)
    {
    try
      {
      URI uri = null;
      if( json != null && json.getValueType() == STRING)
        {
        uri = new URI( ((JsonString) json).getChars().toString());
        }

      return uri;
      }
    catch( Exception e)
      {
      throw new ProjectException( "Error reading base tests definition", e);
      }
    }

  /**
   * Returns the JSON object that represents the given project definition.
   */
  public static JsonObject toJson( Project project)
    {
    JsonObjectBuilder builder = Json.createObjectBuilder();

    Optional.ofNullable( project.getBaseLocation())
      .ifPresent( uri -> builder.add( REFBASE_KEY, String.valueOf( uri)));

    if( project.getSystemInputLocation() != null)
      {
      builder.add( INPUTDEF_KEY, String.valueOf( project.getSystemInputLocation()));
      }
    else if( project.getSystemInput() != null)
      {
      builder.add( INPUTDEF_KEY, SystemInputJson.toJson( project.getSystemInput()));
      }

    if( project.getGeneratorsLocation() != null)
      {
      builder.add( GENERATORS_KEY, String.valueOf( project.getGeneratorsLocation()));
      }
    else if( project.getGenerators() != null)
      {
      builder.add( GENERATORS_KEY, GeneratorSetJson.toJson( project.getGenerators()));
      }

    if( project.getBaseTestsLocation() != null)
      {
      builder.add( BASETESTS_KEY, String.valueOf( project.getBaseTestsLocation()));
      }
    else if( project.getBaseTests() != null)
      {
      builder.add( BASETESTS_KEY, SystemTestJson.toJson( project.getBaseTests()));
      }

    return builder.build();
    }

  private static final String BASETESTS_KEY = "baseTests";
  private static final String GENERATORS_KEY = "generators";
  private static final String INPUTDEF_KEY = "inputDef";
  private static final String REFBASE_KEY = "refBase";
}

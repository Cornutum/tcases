//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2025, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import static org.cornutum.tcases.DefUtils.toIdentifier;

import io.swagger.v3.oas.models.parameters.Parameter;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import static java.util.Collections.emptyMap;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

/**
 * Defines methods for managing request parameters.
 */
public final class ParameterUtils
  {
  /**
   * Creates a new ParameterUtils instance.
   */
  private ParameterUtils()
    {
    // Static methods only
    }

  /**
   * Returns the given parameters annotated with unique variable names.
   */
  public static Stream<Parameter> withUniqueVarName( Stream<Parameter> parameters)
    {
    List<Parameter> annotated = parameters.collect( toList());

    Map<String,List<Parameter>> byName = annotated.stream().collect( groupingBy( Parameter::getName));
    Map<String,List<List<Parameter>>> byIdentifier = byName.values().stream().collect( groupingBy( params -> toIdentifier( params.get(0).getName())));

    byIdentifier.forEach( (id, idParams) -> {
      if( idParams.size() == 1)
        {
        // Only one parameter name maps to this identifier.
        setVarName( idParams.get(0), id);
        }
      else
        {
        // For each parameter name that maps to this identifier, assign a different unique identifier.
        IntStream.range( 0, idParams.size())
          .forEach( i -> setVarName( idParams.get(i), String.format( "%s_V%d_", id, i)));
        }
      });
    
    return annotated.stream();
    }

  /**
   * Set the unique variable name for each the given parameter using the given id.
   */
  private static void setVarName( List<Parameter> parameters, String id)
    {
    if( parameters.size() == 1)
      {
      // Only one parameter with this id.
      setVarName( parameters.get(0), id);
      }
    else
      {
      // Multiple parameters with same name but different locations.
      parameters.forEach( parameter -> setVarName( parameter, ParameterId.stringValue( id, parameter.getIn())));
      }
    }

  /**
   * Changes the unique variable name for the given parameter.
   */
  public static void setVarName( Parameter parameter, String varName)
    {
    parameter.addExtension( EXT_VAR_NAME, varName);
    }

  /**
   * Returns the unique variable name for the given parameter.
   */
  public static String getVarName( Parameter parameter)
    {
    return (String) getExtensions( parameter).get( EXT_VAR_NAME);
    }

  /**
   * Returns the extensions for the given parameter.
   */
  private static Map<String,Object> getExtensions( Parameter parameter)
    {
    return Optional.ofNullable( parameter.getExtensions()).orElse( emptyMap());
    }

  private static final String EXT_VAR_NAME = "x-tcases-var-name";
  }

/////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi;

import org.cornutum.tcases.SystemInputDef;

import io.swagger.v3.oas.models.OpenAPI;

/**
 * Creates a {@link SystemInputDef system input definition} for the API requests defined by examples in the given
 * OpenAPI specification. Returns null if the given spec defines no API request examples to model.
 * <P/>
 * OpenAPI models must conform to <U>OAS version 3</U>. See <A href="https://swagger.io/specification/#specification">https://swagger.io/specification/#specification</A>.
 */
public class RequestExamplesModeller extends InputModeller
  {
  /**
   * Creates a new RequestExampleModeller instance.
   */
  public RequestExamplesModeller()
    {
    super( View.REQUEST);
    }
  
  /**
   * Creates a new RequestExampleModeller instance.
   */
  public RequestExamplesModeller( ModelOptions options)
    {
    super( View.REQUEST, options);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by examples in the given
   * OpenAPI specification. Returns null if the given spec defines no API request examples to model.
   */
  public SystemInputDef getRequestExamplesModel( OpenAPI api)
    {
    return requestExamplesModel( api);
    }
  }

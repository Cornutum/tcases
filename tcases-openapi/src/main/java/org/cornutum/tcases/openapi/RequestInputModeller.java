/////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////
package org.cornutum.tcases.openapi;

import org.cornutum.tcases.SystemInputDef;

import io.swagger.v3.oas.models.OpenAPI;

/**
 * Creates a {@link SystemInputDef system input definition} for the API requests defined by the given
 * OpenAPI definition. Returns null if the given definition defines no API requests to model.
 * <P/>
 * OpenAPI models must conform to <U>OAS version 3</U>. See <A href="https://swagger.io/specification/#specification">https://swagger.io/specification/#specification</A>.
 */
public class RequestInputModeller extends InputModeller
  {
  /**
   * Creates a new RequestInputModeller instance.
   */
  public RequestInputModeller()
    {
    super( View.REQUEST);
    }
  
  /**
   * Creates a new RequestInputModeller instance.
   */
  public RequestInputModeller( ModelOptions options)
    {
    super( View.REQUEST, options);
    }

  /**
   * Returns a {@link SystemInputDef system input definition} for the API requests defined by the given
   * OpenAPI definition. Returns null if the given definition defines no API requests to model.
   */
  public SystemInputDef getRequestInputModel( OpenAPI api)
    {
    return requestInputModel( api);
    }
  }

//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2012, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.io;

import org.cornutum.tcases.*;
import org.cornutum.tcases.conditions.*;

import static org.cornutum.tcases.DefUtils.*;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * An {@link ISystemInputSource} that reads from an XML document.
 *
 * @version $Revision$, $Date$
 */
public class SystemInputDocReader extends DefaultHandler implements ISystemInputSource
  {
  /**
   * The base class for element handlers used by this parser.
   *
   * @version $Revision$, $Date$
   */
  protected class ElementHandler extends DefaultHandler
    {
    /**
     * Returns the value of the given attribute. Throws a SAXException if the attribute is not defined.
     */
    public String requireAttribute( Attributes attributes, String attributeName, String attributeValue) throws SAXException
      {
      if( attributeValue == null)
        {
        throw new SAXParseException( "No \"" + attributeName + "\" attribute specified", getDocumentLocator()); 
        }

      return attributeValue;
      }

    /**
     * Returns the value of the given attribute. Throws a SAXException if the attribute is undefined or empty.
     */
    public String requireAttribute( Attributes attributes, String attributeName) throws SAXException
      {
      return requireAttribute( attributes, attributeName, getAttribute( attributes, attributeName));
      }

    /**
     * Returns the value of the given identifier attribute or null if undefined. Throws a SAXException if the attribute is invalid.
     */
    public String getIdentifier( Attributes attributes, String attributeName) throws SAXException
      {
      return toIdentifier( attributeName, getAttribute( attributes, attributeName));
      }

    /**
     * Returns the value of the given identifier attribute. Throws a SAXException if the attribute is undefined, empty, or invalid.
     */
    public String requireIdentifier( Attributes attributes, String attributeName) throws SAXException
      {
      return toIdentifier( attributeName, requireAttribute( attributes, attributeName));
      }

    /**
     * Returns the given attribute value as an identifier. Throws a SAXException if the attribute is not a valid identifier.
     */
    public String toIdentifier( String attributeName, String attributeValue) throws SAXException
      {
      String id = StringUtils.trimToNull( attributeValue);

      if( id != null)
        {
        try
          {
          assertIdentifier( id);
          }
        catch( Exception e)
          {
          throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute: " + e.getMessage(), getDocumentLocator()); 
          }
        }

      return id;
      }
      
    /**
     * Returns the value of the given attribute. Returns null if the attribute value is undefined or empty.
     */
    public String getAttribute( Attributes attributes, String attributeName)
      {
      String value = attributes.getValue( attributeName);
      return StringUtils.isBlank( value)? null : value;
      }

    /**
     * Records and returns a set of property definitions.
     */
    public Set<String> propertiesDefined( Set<String> properties)
      {
      if( properties != null)
        {
        FunctionHandler functionHandler = getFunctionHandler();
        Integer currentLine = getDocumentLocator().getLineNumber();
        for( String property : properties)
          {
          functionHandler.addPropertyDef( property, currentLine);
          }
        }
      
      return properties;
      }

    /**
     * Records and returns a set of property references.
     */
    public Set<String> propertiesReferenced( Set<String> properties)
      {
      if( properties != null)
        {
        FunctionHandler functionHandler = getFunctionHandler();
        Integer currentLine = getDocumentLocator().getLineNumber();
        for( String property : properties)
          {
          functionHandler.addPropertyRef( property, currentLine);
          }
        }
      
      return properties;
      }

    /**
     * Converts the given attribute value to a set of property names.
     */
    public Set<String> toProperties( Attributes attributes, String attributeName) throws SAXParseException
      {
      Set<String> propertySet = null;

      String propertyList = StringUtils.strip( getAttribute( attributes, attributeName));
      String[] properties = propertyList==null? null : propertyList.split( ",");
      if( properties != null && properties.length > 0)
        {
        propertySet = new HashSet<String>();
        for( int i = 0; i < properties.length; i++)
          {
          String propertyName = StringUtils.trimToNull( properties[i]);
          if( propertyName != null)
            {
            propertySet.add( propertyName);
            }
          }

        try
          {
          assertPropertyIdentifiers( propertySet);
          }
        catch( Exception e)
          {
          throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute: " + e.getMessage(), getDocumentLocator()); 
          }
        }
      
      return propertySet;
      }

    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return false;
      }
      
    /**
     * Changes the parent ElementHandler.
     */
    public void setParent( ElementHandler parent)
      {
      parent_ = parent;
      }

    /**
     * Returns the parent ElementHandler.
     */
    public ElementHandler getParent()
      {
      return parent_;
      }

    /**
     * Returns the handler for the Function element containing this element.
     */
    public FunctionHandler getFunctionHandler()
      {
      // By default, same as parent's FunctionHandler.
      return
        getParent() == null
        ? null
        : getParent().getFunctionHandler();
      }

    private ElementHandler parent_;
    }
  
  /**
   * Handles System elements.
   *
   * @version $Revision$, $Date$
   */
  protected class SystemHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return FUNCTION_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setSystemInputDef( new SystemInputDef( requireIdentifier( attributes, NAME_ATR)));
      }

    /**
     * Changes the {@link SystemInputDef} represented by this element.
     */
    private void setSystemInputDef( SystemInputDef inputDef)
      {
      systemInputDef_ = inputDef;
      }

    /**
     * Returns the {@link SystemInputDef} represented by this element.
     */
    public SystemInputDef getSystemInputDef()
      {
      return systemInputDef_;
      }
    }
  
  /**
   * Handles Function elements.
   *
   * @version $Revision$, $Date$
   */
  protected class FunctionHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return INPUT_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setFunctionInputDef( new FunctionInputDef( requireIdentifier( attributes, NAME_ATR)));
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      FunctionInputDef functionInputDef = getFunctionInputDef();
      if( functionInputDef.getVarTypes().length == 0)
        {
        throw new SAXParseException( "No input variables defined for " + functionInputDef, getDocumentLocator()); 
        }

      // Any properties defined but not referenced?
      List<String> propertiesUnused = getPropertiesUnexpected( propertyRefs_, propertyDefs_);
      for( String property : propertiesUnused)
        {
        // Yes, log a warning.
        int lineNumber = propertyDefs_.get( property);
        logger_.warn( "line={}: property={} is defined but never used", lineNumber, property);
        }
      
      // Any properties referenced but not defined?
      List<String> propertiesUnknown = getPropertiesUnexpected( propertyDefs_, propertyRefs_);
      for( String property : propertiesUnknown)
        {
        // Yes, report a failure.
        int lineNumber = propertyRefs_.get( property);
        throw
          new SAXParseException
          ( "Property=" + property + " is undefined",
            getDocumentLocator().getPublicId(),
            getDocumentLocator().getSystemId(),
            lineNumber,
            1);
        }
      
      SystemHandler parent = (SystemHandler) getParent();
      try
        {
        parent.getSystemInputDef().addFunctionInputDef( functionInputDef);
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Can't add definition for " + functionInputDef, getDocumentLocator(), e); 
        }
      }

    /**
     * Changes the {@link FunctionInputDef} represented by this element.
     */
    private void setFunctionInputDef( FunctionInputDef functionInputDef)
      {
      functionInputDef_ = functionInputDef;
      }

    /**
     * Returns the {@link FunctionInputDef} represented by this element.
     */
    public FunctionInputDef getFunctionInputDef()
      {
      return functionInputDef_;
      }

    /**
     * Returns the handler for the Function element containing this element.
     */
    public FunctionHandler getFunctionHandler()
      {
      return this;
      }

    /**
     * Records the location of a property definition.
     */
    public void addPropertyDef( String property, Integer lineNum)
      {
      if( !propertyDefs_.containsKey( property))
        {
        propertyDefs_.put( property, lineNum);
        }
      }

    /**
     * Records the location of a property reference.
     */
    public void addPropertyRef( String property, Integer lineNum)
      {
      if( !propertyRefs_.containsKey( property))
        {
        propertyRefs_.put( property, lineNum);
        }
      }

    /**
     * Returns the set of unexpected property names, ordered by increasing line number.
     */
    private List<String> getPropertiesUnexpected( Map<String,Integer> expected, final Map<String,Integer> actual)
      {
      List<String> unexpected = new ArrayList<String>( CollectionUtils.subtract( actual.keySet(), expected.keySet()));
      Collections.sort
        ( unexpected,
          new Comparator<String>()
            {
            public int compare( String property1, String property2)
              {
              int line1 = actual.get( property1);
              int line2 = actual.get( property2);
              return line1 - line2;
              }
            });
      return unexpected;
      }

    private FunctionInputDef functionInputDef_;
    private Map<String,Integer> propertyDefs_ = new HashMap<String,Integer>();
    private Map<String,Integer> propertyRefs_ = new HashMap<String,Integer>();
    }
  
  /**
   * Handles Input elements.
   *
   * @version $Revision$, $Date$
   */
  protected class InputHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return
        VAR_TAG.equals( memberQname)
        || VARSET_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      String type = getIdentifier( attributes, TYPE_ATR);
      setType( type == null? IVarDef.ARG : type);
      }

    /**
     * Changes the input variable type represented by this element.
     */
    private void setType( String type)
      {
      type_ = type;
      }

    /**
     * Returns the input variable type represented by this element.
     */
    public String getType()
      {
      return type_;
      }

    /**
     * Returns the {@link FunctionInputDef} for this element.
     */
    public FunctionInputDef getFunctionInputDef()
      {
      FunctionHandler parent = (FunctionHandler) getParent();
      return parent.getFunctionInputDef();
      }

    private String type_;
    }
  
  /**
   * Base class for conditional elements.
   *
   * @version $Revision$, $Date$
   */
  protected abstract class ConditionalHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return WHEN_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      ICondition condition = null;
      
      Set<String> when = propertiesReferenced( toProperties( attributes, WHEN_ATR));
      if( when != null)
        {
        condition = new ContainsAll( when);
        }

      Set<String> whenNot = propertiesReferenced( toProperties( attributes, WHENNOT_ATR));
      if( whenNot != null)
        {
        ICondition excludes = new Not().add( new ContainsAny( whenNot));

        condition =
          condition == null
          ? excludes
          : new AllOf().add( condition).add( excludes);
        }

      getConditional().setCondition( condition);
      }

    /**
     * Returns the {@link Conditional} represented by this element.
     */
    public abstract Conditional getConditional();
    }
  
  /**
   * Base class for elements that can contain one or more conditions.
   *
   * @version $Revision$, $Date$
   */
  protected abstract class ConditionContainer extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return
        ALLOF_TAG.equals( memberQname)
        || ANYOF_TAG.equals( memberQname)
        || NOT_TAG.equals( memberQname);
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      if( isEmpty())
        {
        throw new SAXParseException( "No member defined for this " + qName + " condition", getDocumentLocator());
        }
      }
    
    /**
     * Adds the  {@link ICondition} to this container.
     */
    public abstract void addCondition( ICondition condition) throws SAXParseException;
    
    /**
     * Returns true if no {@link ICondition} added to this container.
     */
    public abstract boolean isEmpty();
    }
  
  /**
   * Handles When elements.
   *
   * @version $Revision$, $Date$
   */
  protected class WhenHandler extends ConditionContainer
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      if( getConditional().getCondition() != null)
        {
        throw new SAXParseException( "Condition already defined for this element", getDocumentLocator()); 
        }
      }
    
    /**
     * Adds the  {@link ICondition} to this container.
     */
    public void addCondition( ICondition condition) throws SAXParseException
      {
      Conditional conditional = getConditional();
      if( conditional.getCondition() != null)
        {
        throw new SAXParseException( "Condition already defined for this element", getDocumentLocator()); 
        }

      conditional.setCondition( condition);
      }
    
    /**
     * Returns true if no {@link ICondition} added to this container.
     */
    public boolean isEmpty()
      {
      return getConditional().getCondition() == null;
      }

    /**
     * Returns the {@link Conditional} that contains by this element.
     */
    public Conditional getConditional()
      {
      ConditionalHandler parent = (ConditionalHandler) getParent();
      return parent.getConditional();
      }
    }
  
  /**
   * Base class for {@link ConditionSet} elements.
   *
   * @version $Revision$, $Date$
   */
  protected abstract class ConditionSetHandler extends ConditionContainer
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      Set<String> properties = propertiesReferenced( toProperties( attributes, PROPERTY_ATR));
      if( properties != null)
        {
        withProperties( properties);
        }
      }

    /**
     * Initializes this ConditionSet using the given set of properties.
     */
    public abstract void withProperties( Set<String> properties) throws SAXParseException;
    
    /**
     * Adds the  {@link ICondition} to this container.
     */
    public void addCondition( ICondition condition) throws SAXParseException
      {
      getConditionSet().add( condition);
      }
    
    /**
     * Returns true if no {@link ICondition} added to this container.
     */
    public boolean isEmpty()
      {
      return getConditionSet().getConditions().hasNext() == false;
      }

    /**
     * Changes the condition set represented by this element.
     */
    public void setConditionSet( ConditionSet conditionSet)
      {
      conditionSet_ = conditionSet;
      }

    /**
     * Returns the condition set represented by this element.
     */
    public ConditionSet getConditionSet()
      {
      return conditionSet_;
      }

    private ConditionSet conditionSet_;
    }
  
  /**
   * Handles AllOf elements.
   *
   * @version $Revision$, $Date$
   */
  protected class AllOfHandler extends ConditionSetHandler
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setConditionSet( new AllOf());
      super.startElement( uri, localName, qName, attributes);
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      super.endElement( uri, localName, qName);
      ConditionContainer parent = (ConditionContainer) getParent();
      parent.addCondition( (AllOf) getConditionSet());
      }

    /**
     * Initializes this ConditionSet using the given set of properties.
     */
    public void withProperties( Set<String> properties) throws SAXParseException
      {
      addCondition( new ContainsAll( properties));
      }
    }
  
  /**
   * Handles AnyOf elements.
   *
   * @version $Revision$, $Date$
   */
  protected class AnyOfHandler extends ConditionSetHandler
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setConditionSet( new AnyOf());
      super.startElement( uri, localName, qName, attributes);
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      super.endElement( uri, localName, qName);
      ConditionContainer parent = (ConditionContainer) getParent();
      parent.addCondition( (AnyOf) getConditionSet());
      }

    /**
     * Initializes this ConditionSet using the given set of properties.
     */
    public void withProperties( Set<String> properties) throws SAXParseException
      {
      addCondition( new ContainsAny( properties));
      }
    }
  
  /**
   * Handles Not elements.
   *
   * @version $Revision$, $Date$
   */
  protected class NotHandler extends ConditionSetHandler
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setConditionSet( new Not());
      super.startElement( uri, localName, qName, attributes);
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      super.endElement( uri, localName, qName);
      ConditionContainer parent = (ConditionContainer) getParent();
      parent.addCondition( (Not) getConditionSet());
      }

    /**
     * Initializes this ConditionSet using the given set of properties.
     */
    public void withProperties( Set<String> properties) throws SAXParseException
      {
      addCondition( new ContainsAny( properties));
      }
    }
  
  /**
   * Base class for variable definition elements.
   *
   * @version $Revision$, $Date$
   */
  protected abstract class VarDefHandler extends ConditionalHandler
    {
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setVarDef( createVarDef( requireIdentifier( attributes, NAME_ATR)));
      super.startElement( uri, localName, qName, attributes);
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      AbstractVarDef varDef = getVarDef();
      try
        {
        if( getParent() instanceof InputHandler)
          {
          InputHandler parent = (InputHandler) getParent();
          varDef.setType( parent.getType());
          parent.getFunctionInputDef().addVarDef( varDef);
          }
        else
          {
          VarSetHandler parent = (VarSetHandler) getParent();
          parent.getVarSet().addMember( varDef);
          }
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Can't add definition for " + varDef, getDocumentLocator(), e); 
        }
      }

    /**
     * Changes the {@link AbstractVarDef} represented by this element.
     */
    private void setVarDef( AbstractVarDef varDef)
      {
      varDef_ = varDef;
      }

    /**
     * Returns the {@link AbstractVarDef} represented by this element.
     */
    public AbstractVarDef getVarDef()
      {
      return varDef_;
      }

    /**
     * Creates the {@link AbstractVarDef} represented by this element.
     */
    public abstract AbstractVarDef createVarDef( String name);

    /**
     * Returns the {@link Conditional} represented by this element.
     */
    public Conditional getConditional()
      {
      return getVarDef();
      }

    private AbstractVarDef varDef_;
    }
  
  /**
   * Handles Var elements.
   *
   * @version $Revision$, $Date$
   */
  protected class VarHandler extends VarDefHandler
    {
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      if( !getVar().getValidValues().hasNext())
        {
        throw new SAXParseException( "No valid values defined for " + getVar(), getDocumentLocator()); 
        }

      super.endElement( uri, localName, qName);
      }
    
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || VALUE_TAG.equals( memberQname);
      }
    
    /**
     * Creates the {@link AbstractVarDef} represented by this element.
     */
    public AbstractVarDef createVarDef( String name)
      {
      return new VarDef( name);
      }

    /**
     * Returns the {@link VarDef} represented by this element.
     */
    public VarDef getVar()
      {
      return (VarDef) getVarDef();
      }
    }
  
  /**
   * Handles VarSet elements.
   *
   * @version $Revision$, $Date$
   */
  protected class VarSetHandler extends VarDefHandler
    {
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      if( !getVarSet().getMembers().hasNext())
        {
        throw new SAXParseException( "No members defined for " + getVarSet(), getDocumentLocator()); 
        }

      super.endElement( uri, localName, qName);
      }
    
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || VAR_TAG.equals( memberQname)
        || VARSET_TAG.equals( memberQname);
      }
    
    /**
     * Creates the {@link AbstractVarDef} represented by this element.
     */
    public AbstractVarDef createVarDef( String name)
      {
      return new VarSet( name);
      }

    /**
     * Returns the {@link VarSet} represented by this element.
     */
    public VarSet getVarSet()
      {
      return (VarSet) getVarDef();
      }
    }
  
  /**
   * Handles Value elements
   *
   * @version $Revision$, $Date$
   */
  protected class ValueHandler extends ConditionalHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || PROPERTY_TAG.equals( memberQname);
      }
    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      VarValueDef value = new VarValueDef( requireIdentifier( attributes, NAME_ATR));
      setValue( value);

      super.startElement( uri, localName, qName, attributes);

      String failureAtr = StringUtils.trimToNull( getAttribute( attributes, FAILURE_ATR));
      boolean failure = failureAtr != null && BooleanUtils.toBoolean( failureAtr);

      String onceAtr = StringUtils.trimToNull( getAttribute( attributes, ONCE_ATR));
      boolean once = onceAtr != null && BooleanUtils.toBoolean( onceAtr);

      value.setType
        ( failure? VarValueDef.Type.FAILURE :
          once? VarValueDef.Type.ONCE :
          VarValueDef.Type.VALID);

      if( getAttribute( attributes, PROPERTY_ATR) != null && !value.getType().isValid())
        {
        throw new SAXParseException( "Can't define properties for a failure value", getDocumentLocator()); 
        }

      value.addProperties( propertiesDefined( toProperties( attributes, PROPERTY_ATR)));
      }

    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      VarHandler parent = (VarHandler) getParent();
      VarDef var = parent.getVar();
      try
        {
        var.addValue( getValue());
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Can't add value for " + var, getDocumentLocator(), e); 
        }
      }

    /**
     * Changes the {@link VarValueDef} represented by this element.
     */
    private void setValue( VarValueDef value)
      {
      value_ = value;
      }

    /**
     * Returns the {@link VarValueDef} represented by this element.
     */
    public VarValueDef getValue()
      {
      return value_;
      }

    /**
     * Returns the {@link Conditional} represented by this element.
     */
    public Conditional getConditional()
      {
      return getValue();
      }

    private VarValueDef value_;
    }
  
  /**
   * Handles Property elements
   *
   * @version $Revision$, $Date$
   */
  protected class PropertyHandler extends ElementHandler
    {    
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      ValueHandler parent = (ValueHandler) getParent();
      VarValueDef value = parent.getValue();

      Set<String> properties = propertiesDefined( toProperties( attributes, NAME_ATR));
      if( properties == null || properties.isEmpty())
        {
        throw new SAXParseException( "No property names specified", getDocumentLocator()); 
        }

      if( !value.getType().isValid())
        {
        throw new SAXParseException( "Can't define properties for a failure value", getDocumentLocator()); 
        }

      value.addProperties( properties);
      }
    }
  
  /**
   * Creates a new SystemInputDocReader object.
   */
  public SystemInputDocReader()
    {
    this( null);
    }
  
  /**
   * Creates a new SystemInputDocReader object.
   */
  public SystemInputDocReader( InputStream stream)
    {
    setInputStream( stream);
    }

  /**
   * Returns a {@link SystemInputDef} instance.
   */
  public SystemInputDef getSystemInputDef()
    {
    SAXParser parser;
    try
      {
      parser = SAXParserFactory.newInstance().newSAXParser();
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't create SAXParser", e);
      }

    try
      {
      parser.parse( getInputStream(), this);
      }
    catch( SAXParseException spe)
      {
      throw
        new RuntimeException
        ( "Error in document at line=" + spe.getLineNumber(),
          spe);
      }
    catch( Exception e)
      {
      throw new RuntimeException( "Can't read SystemInputDef", e);
      }

    return systemInputDef_;
    }

  /**
   * Changes the input stream for this reader.
   */
  public void setInputStream( InputStream stream)
    {
    stream_ =
      stream==null
      ? System.in
      : stream;
    }

  /**
   * Returns the input stream for this reader.
   */
  protected InputStream getInputStream()
    {
    return stream_;
    }

  /**
   * Pushes the given ElementHandler onto the top of the parse stack.
   */
  protected void pushElementHandler( ElementHandler handler)
    {
    elementHandlers_.add( 0, handler);
    }

  /**
   * Removes an ElementHandler from the top of the parse stack.
   */
  protected ElementHandler popElementHandler()
    {
    return elementHandlers_.size() > 0? elementHandlers_.remove( 0) : null;
    }

  /**
   * Returns the ElementHandler at the top of the parse stack.
   */
  protected ElementHandler getCurrentElementHandler()
    {
    return elementHandlers_.size() > 0? elementHandlers_.get( 0) : null;
    }
    
  public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
    {
    ElementHandler handler =
      qName.equals( ALLOF_TAG)?       (ElementHandler) new AllOfHandler() :
      qName.equals( ANYOF_TAG)?       (ElementHandler) new AnyOfHandler() :
      qName.equals( FUNCTION_TAG)?    (ElementHandler) new FunctionHandler() :
      qName.equals( INPUT_TAG)?       (ElementHandler) new InputHandler() :
      qName.equals( NOT_TAG)?         (ElementHandler) new NotHandler() :
      qName.equals( PROPERTY_TAG)?    (ElementHandler) new PropertyHandler() :
      qName.equals( SYSTEM_TAG)?      (ElementHandler) new SystemHandler() :
      qName.equals( VALUE_TAG)?       (ElementHandler) new ValueHandler() :
      qName.equals( VAR_TAG)?         (ElementHandler) new VarHandler() :
      qName.equals( VARSET_TAG)?      (ElementHandler) new VarSetHandler() :
      qName.equals( WHEN_TAG)?        (ElementHandler) new WhenHandler() :
      null;

    if( handler == null)
      {
      throw new SAXParseException( "Unknown element: " + qName, getDocumentLocator()); 
      }

    ElementHandler parentHandler = getCurrentElementHandler();
    if( !(parentHandler == null
          ? SYSTEM_TAG.equals( qName)
          : parentHandler.isMember( qName)))
      {
      throw new SAXParseException( "The " + qName + " element is not allowed at this location", getDocumentLocator()); 
      }

    handler.setParent( parentHandler);
    pushElementHandler( handler);
    handler.startElement( uri, localName, qName, attributes);
    }

  public void endElement( String uri, String localName, String qName) throws SAXException
    {
    ElementHandler handler = getCurrentElementHandler();
    if( handler != null)
      {
      handler.endElement( uri, localName, qName);
      popElementHandler(); 
      }
    }

  public void setDocumentLocator( Locator locator)
    {
    locator_ = locator;
    }

  public Locator getDocumentLocator()
    {
    return locator_;
    }
    
  public void warning( SAXParseException e) throws SAXException
    {
    }

  public void error( SAXParseException e) throws SAXException
    {
    throw e;
    }

  private InputStream           stream_;
  private Locator               locator_;
  private List<ElementHandler>  elementHandlers_  = new ArrayList<ElementHandler>();
  private SystemInputDef        systemInputDef_;

  private static final String ALLOF_TAG     = "AllOf";
  private static final String ANYOF_TAG     = "AnyOf";
  private static final String FUNCTION_TAG  = "Function";
  private static final String INPUT_TAG     = "Input";
  private static final String NOT_TAG       = "Not";
  private static final String PROPERTY_TAG  = "Property";
  private static final String SYSTEM_TAG    = "System";
  private static final String VALUE_TAG     = "Value";
  private static final String VARSET_TAG    = "VarSet";
  private static final String VAR_TAG       = "Var";
  private static final String WHEN_TAG      = "When";

  private static final String FAILURE_ATR   = "failure";
  private static final String NAME_ATR      = "name";
  private static final String ONCE_ATR      = "once";
  private static final String PROPERTY_ATR  = "property";
  private static final String TYPE_ATR      = "type";
  private static final String WHENNOT_ATR   = "whenNot";
  private static final String WHEN_ATR      = "when";

  private static final Logger logger_ = LoggerFactory.getLogger( SystemInputDocReader.class);
  }

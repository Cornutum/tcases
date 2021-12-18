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
import static org.cornutum.tcases.io.SystemInputDoc.*;
import static org.cornutum.tcases.util.ObjectUtils.toObject;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Closeable;
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
 */
public class SystemInputDocReader extends DefaultHandler implements ISystemInputSource, Closeable
  {
  /**
   * The base class for element handlers used by this parser.
   *
   */
  protected abstract class ElementHandler extends DefaultHandler
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
     * Returns the value of the given attribute. Throws a SAXException if the attribute is undefined or empty.
     */
    public String requireNonBlankAttribute( Attributes attributes, String attributeName) throws SAXException
      {
      return requireAttribute( attributes, attributeName, StringUtils.trimToNull(getAttribute( attributes, attributeName)));
      }

    /**
     * Returns the value of the given integer attribute or null if undefined. Throws a SAXException if the attribute is invalid.
     */
    public Integer getInteger( Attributes attributes, String attributeName) throws SAXException
      {
      return toInteger( attributeName, getAttribute( attributes, attributeName));
      }

    /**
     * Returns the value of the given integer attribute. Throws a SAXException if the attribute is undefined, empty, or invalid.
     */
    public Integer requireInteger( Attributes attributes, String attributeName) throws SAXException
      {
      return toInteger( attributeName, requireNonBlankAttribute( attributes, attributeName));
      }

    /**
     * Returns the given attribute value as an integer. Throws a SAXException if the attribute is not a valid integer.
     */
    public Integer toInteger( String attributeName, String attributeValue) throws SAXException
      {
      Integer integer = null;
      String value = StringUtils.trimToNull( attributeValue);

      if( value != null)
        {
        try
          {
          try
            {
            integer = Integer.valueOf( value);
            }
          catch( Exception e)
            {
            throw new RuntimeException( "Invalid value=\"" + value + "\", must be a non-negative integer");
            }
          if( integer < 0)
            {
            throw new RuntimeException( "Invalid value=" + integer + ", must be non-negative");
            }
          }
        catch( Exception e)
          {
          throw new SAXParseException( "Invalid \"" + attributeName + "\" attribute: " + e.getMessage(), getDocumentLocator()); 
          }
        }
      
      return integer;
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
      return toIdentifier( attributeName, requireNonBlankAttribute( attributes, attributeName));
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
      return attributes.getValue( attributeName);
      }
      
    /**
     * Reports an error if the any of the given attributes are not valid for this element.
     */
    public void validateAttributes( String elementName, Attributes attributes) throws SAXException
      {
      Set<String> validAttributes = getValidAttributes();
      for( int i = attributes.getLength() - 1; i >= 0; i--)
        {
        if( !validAttributes.contains( attributes.getQName(i)))
          {
          throw
            new SAXParseException
            ( "Attribute=" + attributes.getQName(i)
              + " is not allowed for " + elementName + " elements",
              getDocumentLocator());
          }
        }
      }
      
    /**
     * Returns the valid attributes for this element.
     */
    protected Set<String> getValidAttributes()
      {
      return addAttributes( new HashSet<String>());
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    protected Set<String> addAttributes( Set<String> attributes)
      {
      // No attributes to add.
      return attributes;
      }
      
    /**
     * Adds the given attribute list.
     */
    protected Set<String> addAttributeList( Set<String> attributes, String... attributeList)
      {
      Collections.addAll( attributes, attributeList);
      return attributes;
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
   * Base class for annotated elements.
   *
   */
  protected abstract class AnnotatedHandler extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return HAS_TAG.equals( memberQname);
      }

    /**
     * Returns the Annotated instance for this handler.
     */
    protected abstract Annotated getAnnotated();
    
    /**
     * Changes the value of the given annotation.
     */
    public void setAnnotation( String name, String value)
      {
      getAnnotated().setAnnotation( name, value);
      }

    /**
     * Returns the value of the given annotation.
     */
    public String getAnnotation( String name)
      {
      return getAnnotated().getAnnotation( name);
      }
    }
  
  /**
   * Handles Has elements
   */
  protected class HasHandler extends ElementHandler
    {    
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      AnnotatedHandler parent = (AnnotatedHandler) getParent();
      try
        {
        String annotation = requireAttribute( attributes, NAME_ATR);
        String value = parent.getAnnotation( annotation);
        if( value != null)
          {
          throw new IllegalArgumentException( "Annotation=" + annotation + " already set to '" + value + "'");
          }
        parent.setAnnotation( annotation, requireAttribute( attributes, VALUE_ATR));
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Can't add annotation: " + e.getMessage(), getDocumentLocator()); 
        }
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), NAME_ATR, VALUE_ATR);
      }
    }
  
  /**
   * Handles System elements.
   *
   */
  protected class SystemHandler extends AnnotatedHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || FUNCTION_TAG.equals( memberQname);
      }
    
    @Override
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
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), NAME_ATR);
      }

    /**
     * Returns the Annotated instance for this handler.
     */
    @Override
    protected Annotated getAnnotated()
      {
      return getSystemInputDef();
      }
    }
  
  /**
   * Handles Function elements.
   *
   */
  protected class FunctionHandler extends AnnotatedHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        ||  INPUT_TAG.equals( memberQname);
      }
    
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setFunctionInputDef( new FunctionInputDef( requireIdentifier( attributes, NAME_ATR)));
      }

    @Override
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
    @Override
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
            @Override
            public int compare( String property1, String property2)
              {
              int line1 = actual.get( property1);
              int line2 = actual.get( property2);
              return line1 - line2;
              }
            });
      return unexpected;
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), NAME_ATR);
      }

    /**
     * Returns the Annotated instance for this handler.
     */
    @Override
    protected Annotated getAnnotated()
      {
      return getFunctionInputDef();
      }

    private FunctionInputDef functionInputDef_;
    private Map<String,Integer> propertyDefs_ = new HashMap<String,Integer>();
    private Map<String,Integer> propertyRefs_ = new HashMap<String,Integer>();
    }
  
  /**
   * Handles Input elements.
   *
   */
  protected class InputHandler extends AnnotatedHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || LIST_TAG.equals( memberQname)
        || SET_TAG.equals( memberQname)
        || VAR_TAG.equals( memberQname)
        || VARSET_TAG.equals( memberQname);
      }
    
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      String type = getIdentifier( attributes, TYPE_ATR);
      setType( type == null? IVarDef.ARG : type);
      }

    @Override
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      for( AbstractVarDef varDef : getVarDefs())
        {
        varDef.addAnnotations( getAnnotated());
        }
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
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), TYPE_ATR);
      }

    /**
     * Returns variable definitions for this element.
     */
    protected List<AbstractVarDef> getVarDefs()
      {
      return varDefs_;
      }

    /**
     * Returns the Annotated instance for this handler.
     */
    @Override
    protected Annotated getAnnotated()
      {
      return annotated_;
      }

    private String type_;
    private List<AbstractVarDef> varDefs_ = new ArrayList<AbstractVarDef>();
    private Annotated annotated_ = new Annotated(){};
    }
  
  /**
   * Base class for conditional elements.
   *
   */
  protected abstract class ConditionalHandler extends AnnotatedHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        ||  WHEN_TAG.equals( memberQname);
      }
    
    @Override
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
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), WHEN_ATR, WHENNOT_ATR);
      }

    /**
     * Returns the Annotated instance for this handler.
     */
    @Override
    protected Annotated getAnnotated()
      {
      return getConditional();
      }
    }
  
  /**
   * Base class for elements that can contain one or more conditions.
   *
   */
  protected abstract class ConditionContainer extends ElementHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return
        ALLOF_TAG.equals( memberQname)
        || ANYOF_TAG.equals( memberQname)
        || NOT_TAG.equals( memberQname)
        || LESSTHAN_TAG.equals( memberQname)
        || NOTLESSTHAN_TAG.equals( memberQname)
        || MORETHAN_TAG.equals( memberQname)
        || NOTMORETHAN_TAG.equals( memberQname)
        || BETWEEN_TAG.equals( memberQname)
        || EQUALS_TAG.equals( memberQname)
        ;
      }

    @Override
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
   */
  protected class WhenHandler extends ConditionContainer
    {
    @Override
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
    @Override
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
    @Override
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
   */
  protected abstract class ConditionSetHandler extends ConditionContainer
    {
    @Override
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
    @Override
    public void addCondition( ICondition condition) throws SAXParseException
      {
      getConditionSet().add( condition);
      }
    
    /**
     * Returns true if no {@link ICondition} added to this container.
     */
    @Override
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
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), PROPERTY_ATR);
      }

    private ConditionSet conditionSet_;
    }
  
  /**
   * Handles AllOf elements.
   *
   */
  protected class AllOfHandler extends ConditionSetHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setConditionSet( new AllOf());
      super.startElement( uri, localName, qName, attributes);
      }

    @Override
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      super.endElement( uri, localName, qName);
      ConditionContainer parent = (ConditionContainer) getParent();
      parent.addCondition( (AllOf) getConditionSet());
      }

    /**
     * Initializes this ConditionSet using the given set of properties.
     */
    @Override
    public void withProperties( Set<String> properties) throws SAXParseException
      {
      addCondition( new ContainsAll( properties));
      }
    }
  
  /**
   * Handles AnyOf elements.
   *
   */
  protected class AnyOfHandler extends ConditionSetHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setConditionSet( new AnyOf());
      super.startElement( uri, localName, qName, attributes);
      }

    @Override
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      super.endElement( uri, localName, qName);
      ConditionContainer parent = (ConditionContainer) getParent();
      parent.addCondition( (AnyOf) getConditionSet());
      }

    /**
     * Initializes this ConditionSet using the given set of properties.
     */
    @Override
    public void withProperties( Set<String> properties) throws SAXParseException
      {
      addCondition( new ContainsAny( properties));
      }
    }
  
  /**
   * Handles Not elements.
   *
   */
  protected class NotHandler extends ConditionSetHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setConditionSet( new Not());
      super.startElement( uri, localName, qName, attributes);
      }

    @Override
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      super.endElement( uri, localName, qName);
      ConditionContainer parent = (ConditionContainer) getParent();
      parent.addCondition( (Not) getConditionSet());
      }

    /**
     * Initializes this ConditionSet using the given set of properties.
     */
    @Override
    public void withProperties( Set<String> properties) throws SAXParseException
      {
      addCondition( new ContainsAny( properties));
      }
    }
  
  /**
   * Base class for cardinality condition elements
   */
  protected abstract class CardinalityHandler extends ElementHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      setProperty( requireNonBlankAttribute( attributes, PROPERTY_ATR));
      propertiesReferenced( Collections.singleton( getProperty()));
      }

    @Override
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      super.endElement( uri, localName, qName);
      ConditionContainer parent = (ConditionContainer) getParent();
      parent.addCondition( getCondition());
      }
    
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), PROPERTY_ATR);
      }

    /**
     * Changes the property referenced by this element.
     */
    private void setProperty( String property)
      {
      property_ = property;
      }

    /**
     * Returns the property referenced by this element.
     */
    public String getProperty()
      {
      return property_;
      }

    /**
     * Changes the condition represented by this element.
     */
    protected void setCondition( ICondition condition)
      {
      condition_ = condition;
      }

    /**
     * Returns the condition represented by this element.
     */
    public ICondition getCondition()
      {
      return condition_;
      }

    private String property_;
    private ICondition condition_;
    }
  
  /**
   * Handles LessThan elements
   */
  protected class LessThanHandler extends CardinalityHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      super.startElement( uri, localName, qName, attributes);
      setCondition(
        new AssertLess(
          getProperty(),
          requireInteger( attributes, MAX_ATR)));
      }
    
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), MAX_ATR);
      }
    }
  
  /**
   * Handles NotLessThan elements
   */
  protected class NotLessThanHandler extends CardinalityHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      super.startElement( uri, localName, qName, attributes);
      setCondition(
        new AssertNotLess(
          getProperty(),
          requireInteger( attributes, MIN_ATR)));
      }
    
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), MIN_ATR);
      }
    }
  
  /**
   * Handles MoreThan elements
   */
  protected class MoreThanHandler extends CardinalityHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      super.startElement( uri, localName, qName, attributes);
      setCondition(
        new AssertMore(
          getProperty(),
          requireInteger( attributes, MIN_ATR)));
      }
    
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), MIN_ATR);
      }
    }
  
  /**
   * Handles NotMoreThan elements
   */
  protected class NotMoreThanHandler extends CardinalityHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      super.startElement( uri, localName, qName, attributes);
      setCondition(
        new AssertNotMore(
          getProperty(),
          requireInteger( attributes, MAX_ATR)));
      }
    
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), MAX_ATR);
      }
    }
  
  /**
   * Handles Between elements
   */
  protected class BetweenHandler extends CardinalityHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      super.startElement( uri, localName, qName, attributes);
      String property = getProperty();
      setCondition(
        new Between(
          getAttribute( attributes, EXCLMIN_ATR) == null
          ? new AssertNotLess( property, requireInteger( attributes, MIN_ATR))
          : new AssertMore( property, requireInteger( attributes, EXCLMIN_ATR)),

          getAttribute( attributes, EXCLMAX_ATR) == null
          ? new AssertNotMore( property, requireInteger( attributes, MAX_ATR))
          : new AssertLess( property, requireInteger( attributes, EXCLMAX_ATR))));
      }
    
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), MIN_ATR, MAX_ATR, EXCLMIN_ATR, EXCLMAX_ATR);
      }
    }
  
  /**
   * Handles Equals elements
   */
  protected class EqualsHandler extends CardinalityHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      super.startElement( uri, localName, qName, attributes);
      setCondition(
        new Equals(
          getProperty(),
          requireInteger( attributes, COUNT_ATR)));
      }
    
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), COUNT_ATR);
      }
    }
  
  /**
   * Base class for variable definition elements.
   *
   */
  protected abstract class VarDefHandler extends ConditionalHandler
    {
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      String varName;
      if( !(getParent() instanceof MembersHandler))
        {
        varName = requireIdentifier( attributes, NAME_ATR);
        }
      else
        {
        if( getAttribute( attributes, NAME_ATR) != null)
          {
          logger_.warn( "line={}: Ignoring superfluous name={} for anonymous member definition", getDocumentLocator().getLineNumber(), getAttribute( attributes, NAME_ATR));
          }
        varName = "Member";
        }

      setVarDef( createVarDef( varName));
      super.startElement( uri, localName, qName, attributes);
      }

    @Override
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      AbstractVarDef varDef = getVarDef();
      try
        {
        if( getParent() instanceof MembersHandler)
          {
          MembersHandler parent = (MembersHandler) getParent();
          if( parent.getVarDef() != null)
            {
            throw new SAXParseException( "Duplicate member definition", getDocumentLocator());             
            }
          parent.setVarDef( varDef);
          }
        else if( getParent() instanceof InputHandler)
          {
          InputHandler parent = (InputHandler) getParent();
          varDef.setType( parent.getType());
          parent.getVarDefs().add( varDef);
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
    protected void setVarDef( AbstractVarDef varDef)
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
    @Override
    public Conditional getConditional()
      {
      return getVarDef();
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), NAME_ATR);
      }

    private AbstractVarDef varDef_;
    }
  
  /**
   * Handles Var elements.
   *
   */
  protected class VarHandler extends VarDefHandler
    {
    @Override
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
    @Override
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || VALUE_TAG.equals( memberQname);
      }
    
    /**
     * Creates the {@link AbstractVarDef} represented by this element.
     */
    @Override
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
   */
  protected class VarSetHandler extends VarDefHandler
    {
    @Override
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
    @Override
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || LIST_TAG.equals( memberQname)
        || SET_TAG.equals( memberQname)
        || VAR_TAG.equals( memberQname)
        || VARSET_TAG.equals( memberQname);
      }
    
    /**
     * Creates the {@link AbstractVarDef} represented by this element.
     */
    @Override
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
   * Handles List elements.
   */
  protected class ListHandler extends VarDefHandler
    {
    @Override
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      if( getListVar().getMemberVarDef() == null)
        {
        throw new SAXParseException( "No member definition specified for " + getListVar(), getDocumentLocator()); 
        }

      super.endElement( uri, localName, qName);
      }
    
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || SIZE_TAG.equals( memberQname)
        || MEMBERS_TAG.equals( memberQname);
      }
    
    /**
     * Creates the {@link AbstractVarDef} represented by this element.
     */
    @Override
    public AbstractVarDef createVarDef( String name)
      {
      return new ListVar( name);
      }

    /**
     * Returns the {@link ListVar} represented by this element.
     */
    public ListVar getListVar()
      {
      return (ListVar) getVarDef();
      }
    }
  
  /**
   * Handles Set elements.
   */
  protected class SetHandler extends ListHandler
    {
    @Override
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      super.endElement( uri, localName, qName);

      SetVar setVar = (SetVar) getListVar();
      try
        {
        setVar.setKey( getKey());
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid key definition", getDocumentLocator(), e);
        }
      }
    
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || KEY_TAG.equals( memberQname);
      }
    
    /**
     * Creates the {@link AbstractVarDef} represented by this element.
     */
    @Override
    public AbstractVarDef createVarDef( String name)
      {
      return new SetVar( name);
      }

    /**
     * Returns the key variables for this set.
     */
    public Set<VarNamePattern> getKey()
      {
      return key_;
      }

    private Set<VarNamePattern> key_ = new HashSet<VarNamePattern>();
    }
  
  /**
   * Handles Size elements
   */
  protected class SizeHandler extends ElementHandler
    {    
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      ListHandler parent = (ListHandler) getParent();
      ListVar listVar = parent.getListVar();

      try
        {
        listVar.setSize( getInteger( attributes, MIN_ATR), getInteger( attributes, MAX_ATR));
        listVar.setMemberCountProperty( getAttribute( attributes, PROPERTY_ATR));
        }
      catch( Exception e)
        {
        throw new SAXParseException( "Invalid size definition", getDocumentLocator(), e);
        }
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), MIN_ATR, MAX_ATR, PROPERTY_ATR);
      }
    }
  
  /**
   * Handles Key elements
   */
  protected class KeyHandler extends ElementHandler
    {  
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return VALUEOF_TAG.equals( memberQname);
      }

    /**
     * Returns the current set key variables.
     */
    public Set<VarNamePattern> getKey()
      {
      SetHandler parent = (SetHandler) getParent();
      return parent.getKey();
      }
    }
  
  /**
   * Handles ValueOf elements
   */
  protected class ValueOfHandler extends ElementHandler
    {    
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      VarNamePattern varPath = new VarNamePattern( requireAttribute( attributes, VAR_ATR));

      KeyHandler parent = (KeyHandler) getParent();
      if( !varPath.isValid())
        {
        throw new SAXParseException( String.format( "Invalid key variable=%s", varPath), getDocumentLocator()); 
        }
      else if( parent.getKey().contains( varPath))
        {
        logger_.warn( "line={}: Ignoring duplicate key variable={}", getDocumentLocator().getLineNumber(), varPath);
        }
      else
        {
        parent.getKey().add( varPath);
        }
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), VAR_ATR);
      }
    }
  
  /**
   * Handles Members elements.
   */
  protected class MembersHandler extends ElementHandler
    {
    @Override
    public void endElement( String uri, String localName, String qName) throws SAXException
      {
      if( getVarDef() == null)
        {
        throw new SAXParseException( "No member definition specified", getDocumentLocator()); 
        }

      if( getVarDef().getCondition() != null)
        {
        logger_.warn( "line={}: Ignoring unsupported variable conditions for member definition", getDocumentLocator().getLineNumber());
        }
      
      ListHandler parent = (ListHandler) getParent();
      parent.getListVar().setMemberVarDef( getVarDef());
      
      super.endElement( uri, localName, qName);
      }
    
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return
        LIST_TAG.equals( memberQname)
        || SET_TAG.equals( memberQname)
        || VAR_TAG.equals( memberQname)
        || VARSET_TAG.equals( memberQname);
      }

    /**
     * Changes the {@link AbstractVarDef} represented by this element.
     */
    public void setVarDef( AbstractVarDef varDef)
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

    private AbstractVarDef varDef_;
    }
  
  /**
   * Handles Value elements
   *
   */
  protected class ValueHandler extends ConditionalHandler
    {
    /**
     * Returns true if the given element is a valid member of this element.
     */
    @Override
    public boolean isMember( String memberQname)
      {
      return
        super.isMember( memberQname)
        || PROPERTY_TAG.equals( memberQname);
      }
    
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      VarValueDef value = new VarValueDef( toObject( requireAttribute( attributes, NAME_ATR)));
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

      if( getAttribute( attributes, PROPERTY_ATR) != null && !value.isValid())
        {
        throw new SAXParseException( "Can't define properties for a failure value", getDocumentLocator()); 
        }

      value.addProperties( propertiesDefined( toProperties( attributes, PROPERTY_ATR)));
      }

    @Override
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
    @Override
    public Conditional getConditional()
      {
      return getValue();
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), NAME_ATR, FAILURE_ATR, ONCE_ATR, PROPERTY_ATR);
      }

    private VarValueDef value_;
    }
  
  /**
   * Handles Property elements
   *
   */
  protected class PropertyHandler extends ElementHandler
    {    
    @Override
    public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
      {
      ValueHandler parent = (ValueHandler) getParent();
      VarValueDef value = parent.getValue();

      Set<String> properties = propertiesDefined( toProperties( attributes, NAME_ATR));
      if( properties == null || properties.isEmpty())
        {
        throw new SAXParseException( "No property names specified", getDocumentLocator()); 
        }

      if( !value.isValid())
        {
        throw new SAXParseException( "Can't define properties for a failure value", getDocumentLocator()); 
        }

      value.addProperties( properties);
      }
      
    /**
     * Adds the valid attributes for this element.
     */
    @Override
    protected Set<String> addAttributes( Set<String> attributes)
      {
      return addAttributeList( super.addAttributes( attributes), NAME_ATR);
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
  @Override
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
    stream_ = stream;
    }

  /**
   * Returns the input stream for this reader.
   */
  protected InputStream getInputStream()
    {
    return
      stream_==null
      ? System.in
      : stream_;
    }

  @Override
  public void close()
    {
    IOUtils.closeQuietly( stream_, null);
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
    
  @Override
  public void startElement( String uri, String localName, String qName, Attributes attributes) throws SAXException
    {
    ElementHandler handler =
      qName.equals( ALLOF_TAG)?       (ElementHandler) new AllOfHandler() :
      qName.equals( ANYOF_TAG)?       (ElementHandler) new AnyOfHandler() :
      qName.equals( BETWEEN_TAG)?     (ElementHandler) new BetweenHandler() :
      qName.equals( EQUALS_TAG)?      (ElementHandler) new EqualsHandler() :
      qName.equals( FUNCTION_TAG)?    (ElementHandler) new FunctionHandler() :
      qName.equals( HAS_TAG)?         (ElementHandler) new HasHandler() :
      qName.equals( INPUT_TAG)?       (ElementHandler) new InputHandler() :
      qName.equals( KEY_TAG)?         (ElementHandler) new KeyHandler() :
      qName.equals( LESSTHAN_TAG)?    (ElementHandler) new LessThanHandler() :
      qName.equals( LIST_TAG)?        (ElementHandler) new ListHandler() :
      qName.equals( MEMBERS_TAG)?     (ElementHandler) new MembersHandler() :
      qName.equals( MORETHAN_TAG)?    (ElementHandler) new MoreThanHandler() :
      qName.equals( NOTLESSTHAN_TAG)? (ElementHandler) new NotLessThanHandler() :
      qName.equals( NOTMORETHAN_TAG)? (ElementHandler) new NotMoreThanHandler() :
      qName.equals( NOT_TAG)?         (ElementHandler) new NotHandler() :
      qName.equals( PROPERTY_TAG)?    (ElementHandler) new PropertyHandler() :
      qName.equals( SET_TAG)?         (ElementHandler) new SetHandler() :
      qName.equals( SIZE_TAG)?        (ElementHandler) new SizeHandler() :
      qName.equals( SYSTEM_TAG)?      (ElementHandler) new SystemHandler() :
      qName.equals( VALUEOF_TAG)?     (ElementHandler) new ValueOfHandler() :
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

    handler.validateAttributes( qName, attributes);
    handler.setParent( parentHandler);
    pushElementHandler( handler);
    handler.startElement( uri, localName, qName, attributes);
    }

  @Override
  public void endElement( String uri, String localName, String qName) throws SAXException
    {
    ElementHandler handler = getCurrentElementHandler();
    if( handler != null)
      {
      handler.endElement( uri, localName, qName);
      popElementHandler(); 
      }
    }

  @Override
  public void setDocumentLocator( Locator locator)
    {
    locator_ = locator;
    }

  public Locator getDocumentLocator()
    {
    return locator_;
    }
    
  @Override
  public void warning( SAXParseException e) throws SAXException
    {
    }

  @Override
  public void error( SAXParseException e) throws SAXException
    {
    throw e;
    }

  private InputStream           stream_;
  private Locator               locator_;
  private List<ElementHandler>  elementHandlers_  = new ArrayList<ElementHandler>();
  private SystemInputDef        systemInputDef_;

  private static final Logger logger_ = LoggerFactory.getLogger( SystemInputDocReader.class);
  }

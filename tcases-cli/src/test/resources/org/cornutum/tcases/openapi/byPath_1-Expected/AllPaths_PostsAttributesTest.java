package org.cornutum.examples;


import org.junit.Test;

import java.util.List;
import java.util.Map;
import static java.util.stream.Collectors.*;

import io.restassured.http.Header;
import io.restassured.response.Response;

import org.cornutum.tcases.openapi.test.ResponseValidator;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class AllPaths_PostsAttributesTest {

    private ResponseValidator responseValidator = new ResponseValidator( getClass());

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=true;subject=A Day In Hell;likes=0")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedValue_Is_False() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes=597033979")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_WhatMeWorry() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=true;subject=What? Me, worry?;likes=0")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                .statusCode( isSuccess())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", "")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", "")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesType_Is_NotObject() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";attributes=-441")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Type=Not object
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";likes=1035843385")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.approved.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved;likes=127128009")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.approved.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesApprovedType_Is_NotBoolean() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=\"+DZYOH4;likes=222686173")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.approved.Type=Not boolean
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesDefined_Is_No() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Defined=No
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesType_Is_NotInteger() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes=true")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Type=Not integer
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesLikesValue_Is_M1() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes=-1")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.likes.Value.Is=-1
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectType_Is_Null() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;subject;likes=279878075")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.subject.Type=null
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesSubjectValue_Is_Other() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;subject=~+:.-g:o)lz~^hL-;. o1\\Vc64a#.Sm!*Y`|qrPD^$*wXi2.a22Qkgt|f8T;#g33QP&F3Sq{<43[l#SP:C>1C~NW?G\\a _%Jkm?u2\"l\"H7*]q:f9@re;likes=859073676")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.subject.Value=Other
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    @Test
    public void tracePostsAttributes_AttributesValuePropertiesAdditional_Is_Yes() {
        Response response =
            given()
                .baseUri( forTestServer())
                .pathParam( "attributes", ";approved=false;likes=776704486;wuf")
            .when()
                .request( "TRACE", "/posts/{attributes}")
            .then()
                // attributes.Value.Properties.Additional=Yes
                .statusCode( isBadRequest())
            .extract()
                .response()
                ;

        responseValidator.assertBodyValid( "TRACE", "/posts/{attributes}", response.statusCode(), response.getContentType(), response.asString());
        responseValidator.assertHeadersValid( "TRACE", "/posts/{attributes}", response.statusCode(), responseHeaders( response));
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer() {
        return forTestServer( null);
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }

    private static Map<String,List<String>> responseHeaders( Response response) {
        return
            response.getHeaders().asList().stream()
            .collect( groupingBy( Header::getName, mapping( Header::getValue, toList())));
    }
}

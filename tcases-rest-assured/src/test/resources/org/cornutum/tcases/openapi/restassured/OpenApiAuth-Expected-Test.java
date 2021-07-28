package org.cornutum.tcases.openapi.restassured;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenApiAuthTest {

    @Test
    public void deleteResource_IdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .header( "Authorization", tcasesApiBasicCredentials())
            .queryParam( "id", "0")
        .when()
            .request( "DELETE", "/resource")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deleteResource_IdDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "Authorization", tcasesApiBearerCredentials())
            .cookie( "apiKey", tcasesApiKey())
        .when()
            .request( "DELETE", "/resource")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deleteResource_IdValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .header( "X-Api-Key", tcasesApiKey())
            .queryParam( "id", "734689226")
        .when()
            .request( "DELETE", "/resource")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void deleteResource_IdType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "Authorization", tcasesApiBasicCredentials())
            .queryParam( "id", (String) null)
        .when()
            .request( "DELETE", "/resource")
        .then()
            // id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deleteResource_IdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "Authorization", tcasesApiBasicCredentials())
            .queryParam( "id", "-352.9")
        .when()
            .request( "DELETE", "/resource")
        .then()
            // id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deleteResource_IdValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .header( "Authorization", tcasesApiBasicCredentials())
            .queryParam( "id", "-1")
        .when()
            .request( "DELETE", "/resource")
        .then()
            // id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void deleteResource_AuthSatisfied_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "DELETE", "/resource")
        .then()
            // Auth.Satisfied=No
            .statusCode( isUnauthorized())
            ;
    }

    @Test
    public void deleteResource_Auth1ApiHttpBearerDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "apiKey", tcasesApiKey())
        .when()
            .request( "DELETE", "/resource")
        .then()
            // Auth.1.apiHttpBearer.Defined=No
            .statusCode( isUnauthorized())
            ;
    }

    @Test
    public void deleteResource_Auth1ApiKeyCookieDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "Authorization", tcasesApiBearerCredentials())
        .when()
            .request( "DELETE", "/resource")
        .then()
            // Auth.1.apiKeyCookie.Defined=No
            .statusCode( isUnauthorized())
            ;
    }

    @Test
    public void getResource() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "GET", "/resource")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postResource_BodyDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "opKey", tcasesApiKey())
            .contentType( "application/json")
            .request().body( "0")
        .when()
            .request( "POST", "/resource")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postResource_BodyApplicationJsonValue_Is_Gt_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "opKey", tcasesApiKey())
            .contentType( "application/json")
            .request().body( "924097999")
        .when()
            .request( "POST", "/resource")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postResource_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "opKey", tcasesApiKey())
        .when()
            .request( "POST", "/resource")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postResource_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer())
            .queryParam( "opKey", tcasesApiKey())
            .contentType( "text/plain")
            .request().body( "-525.6")
        .when()
            .request( "POST", "/resource")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postResource_BodyApplicationJsonType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "opKey", tcasesApiKey())
            .contentType( "application/json")
            .request().body( "null")
        .when()
            .request( "POST", "/resource")
        .then()
            // Body.application-json.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postResource_BodyApplicationJsonType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "opKey", tcasesApiKey())
            .contentType( "application/json")
            .request().body( "\"B_&\"")
        .when()
            .request( "POST", "/resource")
        .then()
            // Body.application-json.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postResource_BodyApplicationJsonValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "opKey", tcasesApiKey())
            .contentType( "application/json")
            .request().body( "-1")
        .when()
            .request( "POST", "/resource")
        .then()
            // Body.application-json.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postResource_AuthOpKeyQueryDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .contentType( "application/json")
            .request().body( "0")
        .when()
            .request( "POST", "/resource")
        .then()
            // Auth.opKeyQuery.Defined=No
            .statusCode( isUnauthorized())
            ;
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static Matcher<Integer> isUnauthorized() {
        return is(401);
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

    private String tcasesApiKey() {
        String apiKey = System.getProperty( "tcasesApiKey");
        return apiKey == null? "" : apiKey;
    }

    private String tcasesApiBearer() {
        String bearer = System.getProperty( "tcasesApiBearer");
        return bearer == null? "" : bearer;
    }

    private String tcasesApiBearerCredentials() {
        return String.format( "Bearer %s", asToken64( tcasesApiBearer()));
    }

    private String tcasesApiUser() {
        String user = System.getProperty( "tcasesApiUser");
        return user == null? "" : user;
    }

    private String tcasesApiPassword() {
        String password = System.getProperty( "tcasesApiPassword");
        return password == null? "" : password;
    }

    private String tcasesApiBasicCredentials() {
        return String.format( "Basic %s", asToken64( String.format( "%s:%s", tcasesApiUser(), tcasesApiPassword())));
    }

    private String asToken64( String value) {
        try {
            return java.util.Base64.getEncoder().encodeToString( value.getBytes( "UTF-8"));
        }
        catch( Exception e) {
            throw new IllegalArgumentException( String.format( "Can't get Base64 token for value=%s", value), e);
        }
    }
}

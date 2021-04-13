describe("if condition handling", () => {
    let container;
    beforeEach(() => {
        container = document.createElement("div");
        document.body.appendChild(container);
    });
    afterEach(() => {
        container.remove();
    });

    it("condition should render the correct result, true first", () => {
        const element = document.createElement("test-components-helper-if-base-index");
        element.foo = true;
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(3);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

        element.foo = false;

        expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
        expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

        element.foo = true

        expect(element.shadowRoot.childNodes.length).toBe(3);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");
    });

    it("condition should render the correct result, false first", () => {
        const element = document.createElement("test-components-helper-if-base-index");
        element.foo = false;
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(3);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
        expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

        element.foo = true;

        expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");

        element.foo = false

        expect(element.shadowRoot.childNodes.length).toBe(3);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("HEADER");
        expect(element.shadowRoot.childNodes[1].tagName).toBe("SPAN");
        expect(element.shadowRoot.childNodes[2].tagName).toBe("FOOTER");
    });
});
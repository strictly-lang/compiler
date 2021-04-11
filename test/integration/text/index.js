describe("host element handling", () => {
    let container;
    beforeEach(() => {
        container = document.createElement("div");
        document.body.appendChild(container);
    });
    afterEach(() => {
        container.remove();
    });

    it("basic text element creation", () => {
        const element = document.createElement("test-components-text-base-index");
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(1);
        expect(element.shadowRoot.childNodes[0].textContent).toBe("foo");
    });

    it("dynamic text", () => {
        const element = document.createElement("test-components-text-dynamic-index");
        element.foo = "bar";
        container.appendChild(element);

        expect(element.shadowRoot.childNodes.length).toBe(1);
        expect(element.shadowRoot.childNodes[0].tagName).toBe("DIV");
        expect(element.shadowRoot.childNodes[0].innerHTML).toBe("con-bar-cat");

        element.foo = "baz";

        expect(element.shadowRoot.childNodes[0].innerHTML).toBe("con-baz-cat");
    });
});